#!/usr/bin/env python
#
# A library that provides a Python interface to the Telegram Bot API
# Copyright (C) 2015-2025
# Leandro Toledo de Souza <devs@python-telegram-bot.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser Public License for more details.
#
# You should have received a copy of the GNU Lesser Public License
# along with this program.  If not, see [http://www.gnu.org/licenses/].
"""This module contains the PendingState."""
import asyncio
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Optional

from telegram._utils.logging import get_logger
from telegram._utils.types import DVType
from telegram.ext._handlers._conversationhandler.conversationdata import ConversationData
from telegram.ext._utils.types import CCT

_LOGGER = get_logger(__name__, class_name="PendingState")

if TYPE_CHECKING:
    from telegram.ext import Application, BaseHandler, ConversationHandler


@dataclass
class PendingState:
    """Thin wrapper around :class:`asyncio.Task` to handle block=False Conversation handlers. Note
    that this is a public class of this module, since :meth:`Application.update_persistence` needs
    to access it. It's still hidden from users, since this module itself is private.
    """

    __slots__ = (
        "_original_conv_key",
        "application",
        "block",
        "context",
        "conv_handler",
        "conversation_data",
        "handler",
        "old_state",
        "task",
        "update",
    )

    def __init__(
        self,
        old_state: object,
        task: asyncio.Task,
        conv_handler: "ConversationHandler[CCT]",
        conversation_data: ConversationData,
        handler: Optional["BaseHandler[Any, CCT, object]"],
        update: object,
        context: CCT,
        application: "Application",
        block: DVType[bool],
    ):
        self.old_state = old_state
        self.task = task
        self.conv_handler = conv_handler
        self.conversation_data = conversation_data
        self.handler = handler
        self.update = update
        self.context = context
        self.application = application
        self.block = block
        self._original_conv_key = conversation_data.key

        # add Method which will be called if the asyncio task is done
        self.task.add_done_callback(lambda done: asyncio.create_task(self.await_new_state(done)))

    async def await_new_state(self, _task: asyncio.Task) -> None:
        await self.conv_handler._update_state(  # pylint: disable=protected-access
            new_state=self._resolve(),
            conversation_data=self.conversation_data,
            update=self.update,
            context=self.context,
            block=self.block,
            application=self.application,
            handler=self.handler,
            original_conv_key=self._original_conv_key,
        )

    def _resolve(self) -> object:
        """Returns the new state of the :class:`telegram.ext.ConversationHandler` if available.
        If there was an exception during the task execution, then return the old state.
        If both the new and old state are :obj:`None`, return `CH.END`. This means Non-blocking
        entry-points with an exception will return `CH.END`.  If only the new state is :obj:`None`,
        return the old state.

        Raises:
            :exc:`RuntimeError`: If the current task has not yet finished.
        """
        # Unfortunately due to circular imports this has to be here
        # pylint: disable=import-outside-toplevel
        from telegram.ext import ConversationHandler

        if not self.task.done():
            raise RuntimeError("New state is not yet available")

        exc = self.task.exception()
        if exc:

            # Special case if an error was raised in a non-blocking entry-point
            if self.old_state is None:
                _LOGGER.exception(
                    "An non-blocking entry-point raised an exception. Ending Conversation."
                )
                return ConversationHandler.END

            _LOGGER.exception(
                "Task function raised exception. Falling back to old state %s",
                self.old_state,
            )
            return self.old_state

        result = self.task.result()

        # a non-blocking entry-point returning None will be Ended
        if result is None and self.old_state is None:
            result = ConversationHandler.END
        elif result is None:
            # returning None from a callback means that we want to stay in the old state
            return self.old_state
        return result

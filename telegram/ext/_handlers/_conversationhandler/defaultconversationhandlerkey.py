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
"""This module contains the DefaultConversationHandlerKey."""
from typing import NoReturn, Union

from telegram import Update
from telegram._utils.warnings import warn
from telegram.ext._handlers._conversationhandler.conversationhandlerkey import (
    ConversationHandlerKey,
)
from telegram.ext._handlers.basehandler import BaseHandler
from telegram.ext._handlers.callbackqueryhandler import CallbackQueryHandler
from telegram.ext._handlers.choseninlineresulthandler import ChosenInlineResultHandler
from telegram.ext._utils.types import CCT, ConversationKey


class DefaultConversationHandlerKey(ConversationHandlerKey):
    """Default implementation for key generation of the :class:`telegram.ext.ConversationHandler`.
    These keys are used to match Updates to a conversation.

    Args:
        per_chat (:obj:`bool`, optional): If the conversation key should contain the Chat's ID.
            Default is :obj:`True`.
        per_user (:obj:`bool`, optional): If the conversation key should contain the User's ID.
            Default is :obj:`True`.
        per_message (:obj:`bool`, optional): If the conversation key should contain the Message's
            ID. Default is :obj:`False`.

    .. versionadded:: NEXT.VERSION
    """

    __slots__ = ("_per_chat", "_per_message", "_per_user")

    def __init__(self, per_chat: bool = True, per_user: bool = True, per_message: bool = False):
        """
        Initialize the DefaultConversationHandlerKey with the given parameters.

        :param per_chat: bool - Whether the function is per chat.
        :param per_user: bool - Whether the function is per user.
        :param per_message: bool - Whether the function is per message.
        """

        self._per_chat = per_chat
        self._per_user = per_user
        self._per_message = per_message

        if not any((self.per_user, self.per_chat, self.per_message)):
            raise ValueError("'per_user', 'per_chat' and 'per_message' can't all be 'False'")

        if self.per_message and not self.per_chat:
            warn(
                "If 'per_message=True' is used, 'per_chat=True' should also be used, "
                "since message IDs are not globally unique.",
                stacklevel=2,
            )

    @property
    def per_user(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the User's ID."""
        return self._per_user

    @per_user.setter
    def per_user(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_user after initialization.")

    @property
    def per_chat(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the Chat's ID."""
        return self._per_chat

    @per_chat.setter
    def per_chat(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_chat after initialization.")

    @property
    def per_message(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the message's ID."""
        return self._per_message

    @per_message.setter
    def per_message(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_message after initialization.")

    def from_update(self, update: object) -> ConversationKey:
        """
        Builds the conversation key associated with the update.

        Parameters:
            update (:obj:`object`): The :class:`telegram.Update` object to build the conversation
                key for.

        Returns:
            ConversationKey: The conversation key associated with the update.
        """
        if not isinstance(update, Update):
            return NotImplemented

        key: list[Union[int, str]] = []

        if self.per_chat:
            if not update.effective_chat:
                return NotImplemented
            key.append(update.effective_chat.id)

        if self.per_user:
            if not update.effective_user:
                return NotImplemented
            key.append(update.effective_user.id)

        if self.per_message:
            if not update.callback_query:
                return NotImplemented
            if update.callback_query.inline_message_id:
                key.append(update.callback_query.inline_message_id)
            elif update.callback_query.message:
                key.append(update.callback_query.message.message_id)
            else:
                return NotImplemented

        return tuple(key)

    def warn_if_handler_is_invalid(self, handler: BaseHandler[Update, CCT, object]) -> None:
        """
        Checks if the Handler is supported with this ConversationHandlerKey. If not, raise a
        Warning. This method will be called on the  initialization of
        :class:`telegram.ext.ConversationHandler` with all added handlers.

        Parameters:
            handler (:obj:`Basehandler`): The handler to check for compatibility.
        """
        # Unfortunately due to circular imports this has to be here
        from telegram.ext import (  # pylint: disable=import-outside-toplevel
            InlineQueryHandler,
            PollAnswerHandler,
            PollHandler,
            PreCheckoutQueryHandler,
            ShippingQueryHandler,
            StringCommandHandler,
            StringRegexHandler,
            TypeHandler,
        )

        # this link will be added to all warnings tied to per_* setting
        per_faq_link = (
            " Read this FAQ entry to learn more about the per_* settings: "
            "https://github.com/python-telegram-bot/python-telegram-bot/wiki"
            "/Frequently-Asked-Questions#what-do-the-per_-settings-in-conversationhandler-do."
        )
        if self.per_chat and (
            isinstance(
                handler,
                (
                    ShippingQueryHandler,
                    InlineQueryHandler,
                    ChosenInlineResultHandler,
                    PreCheckoutQueryHandler,
                    PollAnswerHandler,
                ),
            )
        ):
            warn(
                f"Updates handled by {handler.__class__.__name__} only have information about "
                "the user, so this handler won't ever be triggered if `per_chat=True`."
                f"{per_faq_link}",
                stacklevel=3,
            )

        elif self.per_message and not isinstance(handler, CallbackQueryHandler):
            warn(
                "If 'per_message=True', all entry points, pre_fallbacks, state handlers"
                " and fallbacks must be 'CallbackQueryHandler', since no other handlers "
                f"have a message context.{per_faq_link}",
                stacklevel=3,
            )
        elif not self.per_message and isinstance(handler, CallbackQueryHandler):
            warn(
                "If 'per_message=False', 'CallbackQueryHandler' will not be "
                f"tracked for every message.{per_faq_link}",
                stacklevel=3,
            )

        elif isinstance(handler, (StringCommandHandler, StringRegexHandler)):
            warn(
                "The `ConversationHandler` only handles updates of type `telegram.Update`. "
                f"{handler.__class__.__name__} handles updates of type `str`.",
                stacklevel=3,
            )
        elif isinstance(handler, TypeHandler) and not issubclass(handler.type, Update):
            warn(
                "The `ConversationHandler` only handles updates of type `telegram.Update`."
                f" The TypeHandler is set to handle {handler.type.__name__}.",
                stacklevel=3,
            )
        elif isinstance(handler, PollHandler):
            warn(
                "PollHandler will never trigger in a conversation since it has no information "
                "about the chat or the user who voted in it. Do you mean the "
                "`PollAnswerHandler`?",
                stacklevel=3,
            )

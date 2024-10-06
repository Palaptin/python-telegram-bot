#!/usr/bin/env python
#
# A library that provides a Python interface to the Telegram Bot API
# Copyright (C) 2015-2024
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
import asyncio
import logging
from typing import Any
from unittest.mock import ANY, Mock

import pytest
from _pytest.logging import LogCaptureFixture

from telegram import Update
from telegram.ext import (
    Application,
    BaseHandler,
    CallbackContext,
    ConversationData,
    ConversationHandler,
    ConversationStates,
    MessageHandler,
)
from telegram.ext._handlers._conversationhandler.pendingstate import PendingState
from tests.auxil.slots import mro_slots


class TestPendingStateWithoutRequest:
    async def test_slot_behaviour(self, app: Application) -> None:
        async with app:
            pending_state = PendingState(
                old_state=None,
                task=asyncio.create_task(app.process_update(Update(0))),
                conv_handler=ConversationHandler(ConversationStates([], {})),
                conversation_data=ConversationData(key=(1, 1), state=None),
                handler=None,
                update=Update(1),
                context=CallbackContext(app),
                application=app,
                block=False,
            )
        for attr in pending_state.__slots__:
            assert getattr(pending_state, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(pending_state)) == len(
            set(mro_slots(pending_state))
        ), "duplicate slot"

    async def test_init(self, app: Application) -> None:

        event = asyncio.Event()

        async def callback(_: Any, __: Any) -> None:
            await event.wait()

        old_state = 1
        task = asyncio.create_task(callback(1, 1))
        conversation_data: ConversationData = ConversationData(key=(1, 1), state=None)
        handler: BaseHandler = MessageHandler(None, callback=callback)
        update = Update(1)
        context = CallbackContext(app)
        application = app
        block = False
        conv_handler: Mock = Mock(ConversationHandler)
        conv_handler.me = 1
        async with app:
            ps = PendingState(
                old_state=old_state,
                task=task,
                conv_handler=conv_handler,
                conversation_data=conversation_data,
                handler=handler,
                update=update,
                context=context,
                application=application,
                block=block,
            )
        assert ps._original_conv_key == conversation_data.key
        assert ps.application is app
        assert ps.block == block
        assert ps.context is context
        assert ps.conv_handler is conv_handler
        assert ps.conversation_data is conversation_data
        assert ps.handler == handler
        assert ps.old_state == old_state
        assert ps.task == task
        assert ps.update == update

        # let the PendingState resolve its task
        event.set()
        await asyncio.sleep(0.1)
        conv_handler._update_state.assert_called_once_with(
            new_state=1,
            conversation_data=conversation_data,
            update=update,
            context=ANY,
            block=block,
            application=app,
            handler=handler,
            original_conv_key=conversation_data.key,
        )

    @pytest.mark.parametrize("old_state", [1, None])
    @pytest.mark.parametrize("new_state", [1, None, RuntimeError])
    async def test_resolve(
        self, app: Application, old_state: Any, new_state: Any, caplog: LogCaptureFixture
    ) -> None:
        event = asyncio.Event()

        async def callback(_: Any, __: Any) -> None:
            await event.wait()
            if new_state is RuntimeError:
                raise RuntimeError
            return new_state

        task = asyncio.create_task(callback(1, 1))
        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(entry_points=[], states={})
        )
        conversation_data: ConversationData = ConversationData(key=(1, 1), state=None)
        handler: BaseHandler = MessageHandler(None, callback=callback)
        update = Update(1)
        context = CallbackContext(app)
        application = app
        block = False
        async with app:
            ps = PendingState(
                old_state=old_state,
                task=task,
                conv_handler=conv_handler,
                conversation_data=conversation_data,
                handler=handler,
                update=update,
                context=context,
                application=application,
                block=block,
            )

        with pytest.raises(RuntimeError, match="New state is not yet available"):
            ps._resolve()

        # let the PendingState resolve its task
        event.set()
        await asyncio.sleep(0.1)

        if new_state is RuntimeError and old_state:
            # On exceptions, we return the old state.
            caplog.clear()
            with caplog.at_level(logging.ERROR):
                assert ps._resolve() == old_state
            assert (
                caplog.records[0].message
                == "Task function raised exception. Falling back to old state 1"
            )
            assert caplog.records[0].name == "telegram.ext.PendingState"

        elif new_state is RuntimeError and not old_state:
            # On exceptions, when the old state is None, we end the conversation (only the case on
            # non-blocking entry-points).
            caplog.clear()
            with caplog.at_level(logging.ERROR):
                assert ps._resolve() == ConversationHandler.END
            assert (
                caplog.records[0].message
                == "An non-blocking entry-point raised an exception. Ending Conversation."
            )
            assert caplog.records[0].name == "telegram.ext.PendingState"

        elif new_state:
            # Use the new state
            assert ps._resolve() == new_state

        elif not new_state and old_state:
            # Keep the old state
            assert ps._resolve() == old_state

        elif not new_state and not old_state:
            # End the conversation
            assert ps._resolve() == ConversationHandler.END

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
"""Persistence of conversations is tested in test_basepersistence.py"""
import asyncio
import datetime
import functools
import logging
from copy import copy
from pathlib import Path
from typing import Any, Callable
from unittest.mock import Mock
from warnings import filterwarnings

import pytest
from _pytest.logging import LogCaptureFixture
from _pytest.monkeypatch import MonkeyPatch
from _pytest.recwarn import WarningsRecorder

from telegram import (
    Bot,
    CallbackQuery,
    Chat,
    ChosenInlineResult,
    InlineQuery,
    Message,
    MessageEntity,
    PreCheckoutQuery,
    ShippingAddress,
    ShippingQuery,
    Update,
    User,
)
from telegram._utils.defaultvalue import DEFAULT_TRUE
from telegram.ext import (
    Application,
    ApplicationBuilder,
    ApplicationHandlerStop,
    CallbackContext,
    CallbackQueryHandler,
    CommandHandler,
    ContextTypes,
    ConversationHandler,
    Defaults,
    JobQueue,
    MessageHandler,
    TypeHandler,
    filters,
)
from telegram.ext._handlers._conversationhandler.conversationstates import ConversationStates
from telegram.ext._handlers._conversationhandler.defaultconversationhandlerkey import (
    DefaultConversationHandlerKey,
)
from telegram.warnings import PTBUserWarning
from tests.auxil.build_messages import make_command_message
from tests.auxil.files import PROJECT_ROOT_PATH
from tests.auxil.pytest_classes import PytestBot, make_bot
from tests.auxil.slots import mro_slots


@pytest.fixture(scope="class")
def user1() -> User:
    return User(first_name="Misses Test", id=123, is_bot=False)


@pytest.fixture(scope="class")
def user2() -> User:
    return User(first_name="Mister Test", id=124, is_bot=False)


def raise_ahs(func: Callable) -> Callable:
    @functools.wraps(func)  # for checking __repr__
    async def decorator(self: Any, *args: Any, **kwargs: Any) -> Any:
        result = await func(self, *args, **kwargs)
        if self.raise_app_handler_stop:
            raise ApplicationHandlerStop(result)
        return result

    return decorator


class TestConversationHandlerWithoutRequest:
    """Persistence of conversations is tested in test_basepersistence.py"""

    # State definitions
    # At first we're thirsty.  Then we brew coffee, we drink it
    # and then we can start coding!
    END, THIRSTY, BREWING, DRINKING, CODING, FIX_PROD = range(-1, 5)

    # Drinking state definitions (nested)
    # At first we're holding the cup.  Then we sip coffee, and last we swallow it
    HOLDING, SIPPING, SWALLOWING, REPLENISHING, STOPPING = map(chr, range(ord("a"), ord("f")))

    current_state: dict = {}
    entry_points: list = []
    states: dict = {}
    pre_fallbacks: list = []
    fallbacks: list = []
    group = Chat(0, Chat.GROUP)
    second_group = Chat(1, Chat.GROUP)

    raise_app_handler_stop = False
    test_flag: Any = False

    # Test related
    @pytest.fixture(autouse=True)
    def _reset(self) -> None:
        self.raise_app_handler_stop = False
        self.test_flag = False
        self.current_state = {}
        self.entry_points = [CommandHandler("start", self.start)]
        self.states = {
            self.THIRSTY: [CommandHandler("brew", self.brew), CommandHandler("wait", self.start)],
            self.BREWING: [CommandHandler("pourCoffee", self.drink)],
            self.DRINKING: [
                CommandHandler("startCoding", self.code),
                CommandHandler("drinkMore", self.drink),
                CommandHandler("end", self.end),
            ],
            self.CODING: [
                CommandHandler("keepCoding", self.code),
                CommandHandler("gettingThirsty", self.start),
                CommandHandler("drinkMore", self.drink),
            ],
        }
        self.state_entry_handlers: dict = {
            self.END: [CommandHandler("end", self.try_to_prevent_end)],
            self.DRINKING: [CommandHandler("pourCoffee", self.drink_is_hot)],
            self.CODING: [CommandHandler("startCoding", self.getting_thirsty)],
        }
        self.pre_fallbacks: list = [CommandHandler("fix_prod", self.fix_prod)]
        self.fallbacks = [CommandHandler("eat", self.start)]
        self.is_timeout = False
        self.state_entry_entered = False

        # for nesting tests
        self.nested_states: dict = {
            self.THIRSTY: [CommandHandler("brew", self.brew), CommandHandler("wait", self.start)],
            self.BREWING: [CommandHandler("pourCoffee", self.drink)],
            self.CODING: [
                CommandHandler("keepCoding", self.code),
                CommandHandler("gettingThirsty", self.start),
                CommandHandler("drinkMore", self.drink),
            ],
        }
        self.drinking_entry_points: list = [CommandHandler("hold", self.hold)]
        self.drinking_states: dict = {
            self.HOLDING: [CommandHandler("sip", self.sip)],
            self.SIPPING: [CommandHandler("swallow", self.swallow)],
            self.SWALLOWING: [CommandHandler("hold", self.hold)],
        }
        self.drinking_fallbacks: list = [
            CommandHandler("replenish", self.replenish),
            CommandHandler("stop", self.stop),
            CommandHandler("end", self.end),
            CommandHandler("startCoding", self.code),
            CommandHandler("drinkMore", self.drink),
        ]
        self.drinking_entry_points.extend(self.drinking_fallbacks)

        # Map nested states to parent states:
        self.drinking_map_to_parent: dict = {
            # Option 1 - Map a fictional internal state to an external parent state
            self.REPLENISHING: self.BREWING,
            # Option 2 - Map a fictional internal state to the END state on the parent
            self.STOPPING: self.END,
            # Option 3 - Map the internal END state to an external parent state
            self.END: self.CODING,
            # Option 4 - Map an external state to the same external parent state
            self.CODING: self.CODING,
            # Option 5 - Map an external state to the internal entry point
            self.DRINKING: self.DRINKING,
        }

    # State handlers
    def _set_state(self, update: Update, state: object) -> object:
        self.current_state[update.message.from_user.id] = state  # type: ignore
        return state

    # Actions
    @raise_ahs
    async def start(self, update: Update, context: ContextTypes.DEFAULT_TYPE) -> object:
        if isinstance(update, Update):
            return self._set_state(update, self.THIRSTY)
        return self._set_state(context.bot, self.THIRSTY)

    @raise_ahs
    async def end(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.END)

    @raise_ahs
    async def start_end(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.END)

    @raise_ahs
    async def start_none(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, None)

    @raise_ahs
    async def brew(self, update: Update, context: ContextTypes.DEFAULT_TYPE) -> object:
        if isinstance(update, Update):
            return self._set_state(update, self.BREWING)
        return self._set_state(context.bot, self.BREWING)

    @raise_ahs
    async def drink_is_hot(self, _: Update, __: ContextTypes.DEFAULT_TYPE) -> None:
        self.state_entry_entered = True

    @raise_ahs
    async def getting_thirsty(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        self.state_entry_entered = True
        return self._set_state(update, self.THIRSTY)

    @raise_ahs
    async def try_to_prevent_end(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        self.state_entry_entered = True
        return self._set_state(update, self.CODING)

    @raise_ahs
    async def drink(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.DRINKING)

    @raise_ahs
    async def code(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.CODING)

    @raise_ahs
    async def fix_prod(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.FIX_PROD)

    @raise_ahs
    async def passout(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> None:
        assert update.message.text == "/brew"  # type: ignore
        assert isinstance(update, Update)
        self.is_timeout = True

    @raise_ahs
    async def passout2(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> None:
        assert isinstance(update, Update)
        self.is_timeout = True

    @raise_ahs
    async def passout_context(self, update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
        assert update.message.text == "/brew"  # type: ignore
        assert isinstance(context, CallbackContext)
        self.is_timeout = True

    @raise_ahs
    async def passout2_context(self, _: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
        assert isinstance(context, CallbackContext)
        self.is_timeout = True

    # Drinking actions (nested)

    @raise_ahs
    async def hold(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.HOLDING)

    @raise_ahs
    async def sip(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.SIPPING)

    @raise_ahs
    async def swallow(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.SWALLOWING)

    @raise_ahs
    async def replenish(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.REPLENISHING)

    @raise_ahs
    async def stop(self, update: Update, _: ContextTypes.DEFAULT_TYPE) -> object:
        return self._set_state(update, self.STOPPING)

    def test_slot_behaviour(self) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(entry_points=[], states={})
        )
        for attr in handler.__slots__:
            assert getattr(handler, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(handler)) == len(set(mro_slots(handler))), "duplicate slot"

    def test_init(self) -> None:
        conversation_states: ConversationStates = ConversationStates(
            entry_points=self.entry_points, states=self.states
        )
        key_builder = DefaultConversationHandlerKey()
        conversation_data_context_type = object()
        ch = ConversationHandler(
            conversation_states=conversation_states,
            key_builder=key_builder,
            conversation_timeout=42,
            name="name",
            persistent="persistent",  # type: ignore
            block="block",  # type: ignore
            conversation_context_data_type=conversation_data_context_type,
        )
        assert ch.available_states is conversation_states
        assert ch.key_builder is key_builder
        assert ch._conversation_timeout == 42
        assert ch.name == "name"
        assert ch.persistent == "persistent"
        assert ch.block is True  # always True, since blocking is handled internally
        assert ch._block == "block"
        assert ch.conversation_context_data_type is conversation_data_context_type

    def test_init_default_values(self) -> None:
        conversation_states: ConversationStates = ConversationStates(
            entry_points=self.entry_points, states=self.states
        )
        key_builder = DefaultConversationHandlerKey()

        ch: ConversationHandler = ConversationHandler(
            conversation_states=conversation_states, key_builder=key_builder
        )

        assert ch.available_states is conversation_states
        assert ch.key_builder is key_builder
        assert ch._conversation_timeout is None
        assert ch.name is None
        assert ch.persistent is False
        assert ch.block is True
        assert ch._block == DEFAULT_TRUE
        assert ch.conversation_context_data_type is None

    def test_init_persistent_no_name(self) -> None:
        with pytest.raises(ValueError, match="can't be persistent when handler is unnamed"):
            ConversationHandler(
                conversation_states=ConversationStates(self.entry_points, states=self.states),
                persistent=True,
            )

    async def test_handlers_generate_warning(self, recwarn: WarningsRecorder) -> None:
        """this function tests all handler + per_* setting combinations."""

        # the warning message action needs to be set to always,
        # otherwise only the first occurrence will be issued
        filterwarnings(action="always", category=PTBUserWarning)
        key_builder = Mock(spec=DefaultConversationHandlerKey)
        recwarn.clear()

        # adding a nested conv to a conversation with timeout should warn
        child: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("code", self.code)],
                states={
                    self.BREWING: [CommandHandler("code", self.code)],
                },
                fallbacks=[CommandHandler("code", self.code)],
            )
        )

        ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("code", self.code)],
                states={
                    self.BREWING: [child],
                },
                fallbacks=[CommandHandler("code", self.code)],
            ),
            conversation_timeout=42,
        )

        # If state is END (-1) we also issue a warning
        ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CallbackQueryHandler(self.code, "code")],
                states={
                    self.END: [CallbackQueryHandler(self.code, "code")],
                },
            ),
            key_builder=key_builder,
        )

        # Check the key builder also had the possibility to raise warnings.
        key_builder.warn_if_handler_is_invalid.assert_called()

        # the overall number of handlers throwing a warning is 13
        assert len(recwarn) == 2

        # now we test the messages, they are raised in the order they are inserted
        # into the conversation handler
        assert (
            str(recwarn[0].message)
            == "Using `conversation_timeout` with nested conversations is currently not "
            "supported. You can still try to use it, but it will likely behave differently"
            " from what you expect."
        )

        assert (
            str(recwarn[1].message)
            == "The END state (-1) is reserved and shouldn't be used as a state name in the "
            "`ConversationHandler`."
        )

        # this for loop checks if the correct stacklevel is used when generating the warning
        for warning in recwarn:
            assert warning.category is PTBUserWarning
            assert warning.filename == __file__, "incorrect stacklevel!"

    def test_repr_no_truncation(self) -> None:
        # ConversationHandler's __repr__ is not inherited from BaseHandler.
        ch: ConversationHandler = ConversationHandler(
            name="test_handler",
            conversation_states=ConversationStates(
                entry_points=[],
                states=self.drinking_states,
            ),
        )
        assert repr(ch) == (
            "ConversationHandler[name=test_handler, "
            "states={'a': [CommandHandler[callback=TestConversationHandlerWithoutRequest.sip]], "
            "'b': [CommandHandler[callback=TestConversationHandlerWithoutRequest.swallow]], "
            "'c': [CommandHandler[callback=TestConversationHandlerWithoutRequest.hold]]}]"
        )

    def test_repr_with_truncation(self) -> None:

        states = copy(self.drinking_states)
        # there are exactly 3 drinking states. adding one more to make sure it's truncated
        states["extra_to_be_truncated"] = [CommandHandler("foo", self.start)]

        ch: ConversationHandler = ConversationHandler(
            name="test_handler",
            conversation_states=ConversationStates(
                entry_points=[],
                states=states,
            ),
        )
        assert repr(ch) == (
            "ConversationHandler[name=test_handler, "
            "states={'a': [CommandHandler[callback=TestConversationHandlerWithoutRequest.sip]], "
            "'b': [CommandHandler[callback=TestConversationHandlerWithoutRequest.swallow]], "
            "'c': [CommandHandler[callback=TestConversationHandlerWithoutRequest.hold]], ...}]"
        )

    async def test_check_update_returns_non(self, app: Application, user1: User) -> None:
        """checks some cases where updates should not be handled"""
        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(entry_points=[], states={}, fallbacks=[]),
            key_builder=DefaultConversationHandlerKey(
                per_message=True,
                per_chat=True,
            ),
        )
        assert not conv_handler.check_update("not an Update")
        assert not conv_handler.check_update(Update(0))
        assert not conv_handler.check_update(
            Update(0, callback_query=CallbackQuery("1", from_user=user1, chat_instance="1"))
        )

    @pytest.mark.parametrize(
        "attr",
        ["name", "persistent", "conversation_timeout"],
        indirect=False,
    )
    def test_immutable(self, attr: str) -> None:
        ch: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(entry_points=[], states={})
        )
        print(attr)
        with pytest.raises(AttributeError, match=f"You can not assign a new value to {attr}"):
            setattr(ch, attr, True)

    @pytest.mark.parametrize("raise_ahs", [True, False])
    async def test_basic_and_app_handler_stop(
        self, app: Application, bot: Bot, user1: User, user2: User, raise_ahs: bool
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)

        async def callback(_: Any, __: Any) -> None:
            self.test_flag = True

        app.add_handler(TypeHandler(object, callback), group=100)
        self.raise_app_handler_stop = raise_ahs

        # User one, starts the state machine.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY
            assert self.test_flag == (not raise_ahs)

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING
            assert self.test_flag == (not raise_ahs)

            # Lets see if an invalid command makes sure, no state is changed.
            message.text = "/nothing"
            message.entities[0].length = len("/nothing")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING
            assert self.test_flag is True
            self.test_flag = False

            # Lets see if the state machine still works by pouring coffee.
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING
            assert self.test_flag == (not raise_ahs)

            # Let's now verify that for another user, who did not start yet,
            # the state has not been changed.
            message.from_user = user2
            await app.process_update(Update(update_id=0, message=message))
            with pytest.raises(KeyError):
                _ = self.current_state[user2.id]

    async def test_conversation_handler_end(
        self, caplog: LogCaptureFixture, app: Application, bot: Bot, user1: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
        )
        app.add_handler(handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/end"
            message.entities[0].length = len("/end")
            caplog.clear()
            with caplog.at_level(logging.ERROR):
                await app.process_update(Update(update_id=0, message=message))
            assert len(caplog.records) == 0
            assert self.current_state[user1.id] == self.END

            # make sure that the conversation has ended by checking that the start command is
            # accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(update_id=0, message=message))

    async def test_conversation_handler_state_entry_handler(
        self, app: Application, bot: Bot, user1: User, user2: User, recwarn: WarningsRecorder
    ) -> None:
        # test if the state_entry_handler is being called.

        # append an end state for this test.
        states = self.states
        states[self.THIRSTY].append(CommandHandler("end", self.end))

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=states,
                state_entry_handlers=self.state_entry_handlers,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)

        # first check if state_entry_handlers will not trigger when not started
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/brew",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/brew"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))
            with pytest.raises(KeyError):
                _ = self.current_state[user1.id]

            # User starts the state machine.
            message.text = "/start"
            message.entities[0].length = len("/start")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING

            # Now we want to pour the coffe but the state_entry_handlers tells us it is hot
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))
            assert self.state_entry_entered is True

            # We nevertheless got something to drink
            assert self.current_state[user1.id] == self.DRINKING

            # Now we want to start coding but get thirsty again in the state_entry_handlers.
            # So the state_entry_handler returned a new state.
            self.state_entry_entered = False
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            await app.process_update(Update(update_id=0, message=message))
            assert self.state_entry_entered is True
            assert self.current_state[user1.id] == self.THIRSTY

            # Now we want to end, but the state_entry_handler prevents this.
            # But he shouldn't be able to change the END state.
            self.state_entry_entered = False
            message.text = "/end"
            message.entities[0].length = len("/end")
            await app.process_update(Update(update_id=0, message=message))
            assert self.state_entry_entered is True
            assert self.current_state[user1.id] == self.CODING

    async def test_conversation_handler_fallback(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)

        # first check if fallback will not trigger start when not started
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/eat",
            entities=[MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/eat"))],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))
            with pytest.raises(KeyError):
                _ = self.current_state[user1.id]

            # User starts the state machine.
            message.text = "/start"
            message.entities[0].length = len("/start")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING

            # Now a fallback command is issued
            message.text = "/eat"
            message.entities[0].length = len("/eat")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY

    async def test_conversation_handler_pre_fallback(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        # extend the states with the pre_fallback command
        states = self.states
        states[self.BREWING].append(CommandHandler("fix_prod", self.start))

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)

        # first check if pre_fallback will not trigger when not started
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/fix_prod",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/fix_prod"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))
            with pytest.raises(KeyError):
                _ = self.current_state[user1.id]

            # User starts the state machine.
            message.text = "/start"
            message.entities[0].length = len("/start")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING

            # Now a pre_fallback command is issued, and the state handler is ignored
            message.text = "/fix_prod"
            message.entities[0].length = len("/fix_prod")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.FIX_PROD

    async def test_unknown_state_warning(
        self, app: Application, bot: Bot, user1: User, recwarn: WarningsRecorder
    ) -> None:
        def build_callback(state: object) -> Callable:
            async def callback(_: Any, __: Any) -> object:
                return state

            return callback

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", build_callback(1))],
                states={
                    1: [TypeHandler(Update, build_callback(69))],
                    2: [TypeHandler(Update, build_callback(42))],
                },
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            name="xyz",
        )
        app.add_handler(handler)
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            try:
                await app.process_update(Update(update_id=1, message=message))
            except Exception as exc:
                print(exc)
                raise exc
            assert len(recwarn) == 1
            assert recwarn[0].category is PTBUserWarning
            assert (
                Path(recwarn[0].filename)
                == PROJECT_ROOT_PATH
                / "telegram"
                / "ext"
                / "_handlers"
                / "_conversationhandler"
                / "conversationhandler.py"
            ), "wrong stacklevel!"
            assert (
                str(recwarn[0].message)
                == "'callback' returned state 69 which is unknown to the ConversationHandler xyz."
            )

    async def test_conversation_handler_per_chat(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            key_builder=DefaultConversationHandlerKey(
                per_user=False,
            ),
        )
        app.add_handler(handler)

        # User one, starts the state machine.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))

            # Let's now verify that for another user, who did not start yet,
            # the state will be changed because they are in the same group.
            message.from_user = user2
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))

            # Check that we're in the DRINKING state by checking that the corresponding command
            # is accepted
            message.from_user = user1
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            assert handler.check_update(Update(update_id=0, message=message))
            message.from_user = user2
            assert handler.check_update(Update(update_id=0, message=message))

    async def test_conversation_handler_per_user(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            key_builder=DefaultConversationHandlerKey(
                per_chat=False,
            ),
        )
        app.add_handler(handler)

        # User one, starts the state machine.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        # First check that updates without user won't be handled
        message.from_user = None
        assert not handler.check_update(Update(update_id=0, message=message))

        message.from_user = user1
        async with app:
            await app.process_update(Update(update_id=0, message=message))

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))

            # Let's now verify that for the same user in a different group, the state will still be
            # updated
            message.chat = self.second_group
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))

            # Check that we're in the DRINKING state by checking that the corresponding command
            # is accepted
            message.chat = self.group
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            assert handler.check_update(Update(update_id=0, message=message))
            message.chat = self.second_group
            assert handler.check_update(Update(update_id=0, message=message))

    @pytest.mark.parametrize("inline", [True, False])
    @pytest.mark.filterwarnings("ignore: If 'per_message=True' is used, 'per_chat=True'")
    async def test_conversation_handler_per_message(
        self, app: Application, bot: Bot, user1: User, user2: User, inline: bool
    ) -> None:
        async def entry(_: Any, __: Any) -> int:
            return 1

        async def one(_: Any, __: Any) -> int:
            return 2

        async def two(_: Any, __: Any) -> int:
            return ConversationHandler.END

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CallbackQueryHandler(entry)],
                states={
                    1: [CallbackQueryHandler(one, pattern="^1$")],
                    2: [CallbackQueryHandler(two, pattern="^2$")],
                },
            ),
            key_builder=DefaultConversationHandlerKey(
                per_message=True,
                per_chat=not inline,
            ),
        )
        app.add_handler(handler)

        # User one, starts the state machine.
        message = (
            Message(
                0,
                datetime.datetime.now(),
                self.group,
                from_user=user1,
                text="msg w/ inlinekeyboard",
            )
            if not inline
            else None
        )
        if message:
            message.set_bot(bot)
            message._unfreeze()
        inline_message_id = "42" if inline else None

        async with app:
            cbq_1 = CallbackQuery(
                "0",
                user1,
                "",
                message=message,
                data="1",
                inline_message_id=inline_message_id,
            )
            cbq_1.set_bot(bot)
            cbq_2 = CallbackQuery(
                "0",
                user1,
                "",
                message=message,
                data="2",
                inline_message_id=inline_message_id,
            )
            cbq_2.set_bot(bot)
            cbq_2._unfreeze()
            await app.process_update(Update(update_id=0, callback_query=cbq_1))

            # Make sure that we're in the correct state
            assert handler.check_update(Update(0, callback_query=cbq_1))
            assert not handler.check_update(Update(0, callback_query=cbq_2))

            await app.process_update(Update(update_id=0, callback_query=cbq_1))

            # Make sure that we're in the correct state
            assert not handler.check_update(Update(0, callback_query=cbq_1))
            assert handler.check_update(Update(0, callback_query=cbq_2))

            # Let's now verify that for a different user in the same group, the state will not be
            # updated
            cbq_2.from_user = user2
            await app.process_update(Update(update_id=0, callback_query=cbq_2))

            cbq_2.from_user = user1
            assert not handler.check_update(Update(0, callback_query=cbq_1))
            assert handler.check_update(Update(0, callback_query=cbq_2))

    async def test_end_on_first_message(self, app: Application, bot: Bot, user1: User) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", self.start_end)], states={}
            )
        )
        app.add_handler(handler)

        # User starts the state machine and immediately ends it.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            assert handler.check_update(Update(update_id=0, message=message))

    async def test_end_on_first_message_non_blocking_handler(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", callback=self.start_end, block=False)],
                states={},
            )
        )
        app.add_handler(handler)

        # User starts the state machine with a non-blocking function that immediately ends the
        # conversation. non-blocking results are resolved when the users state is queried next
        # time.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            # give the task a chance to finish
            await asyncio.sleep(0.1)

            # Let's check that processing the same update again is accepted. this confirms that
            # a) the pending state is correctly resolved
            # b) the conversation has ended
            assert handler.check_update(Update(0, message=message))

    async def test_none_on_first_message(self, app: Application, bot: Bot, user1: User) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[MessageHandler(filters.ALL, self.start_none)], states={}
            )
        )
        app.add_handler(handler)

        # User starts the state machine and a callback function returns None
        message = Message(0, datetime.datetime.now(), self.group, from_user=user1, text="/start")
        message.set_bot(bot)
        message._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            # Check that the same message is accepted again, i.e. the conversation immediately
            # ended
            assert handler.check_update(Update(0, message=message))

    async def test_none_on_first_message_non_blocking_handler(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", self.start_none, block=False)],
                states={},
            )
        )
        app.add_handler(handler)

        # User starts the state machine with a non-blocking handler that returns None
        # non-blocking results are resolved when the users state is queried next time.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            text="/start",
            from_user=user1,
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            # Give the task a chance to finish
            await asyncio.sleep(0.1)

            # Let's check that processing the same update again is accepted. this confirms that
            # a) the pending state is correctly resolved
            # b) the conversation has ended
            assert handler.check_update(Update(0, message=message))

    async def test_per_chat_message_without_chat(self, bot: Bot, user1: User) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", self.start_end)], states={}
            )
        )
        cbq = CallbackQuery("0", user1, "", None)
        cbq.set_bot(bot)
        update = Update(0, callback_query=cbq)
        assert not handler.check_update(update)

    async def test_channel_message_without_chat(self, bot: Bot) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[MessageHandler(filters.ALL, self.start_end)], states={}
            )
        )
        message = Message(
            0, date=datetime.datetime.now(), chat=Chat(0, Chat.CHANNEL, "Misses Test")
        )
        message.set_bot(bot)
        message._unfreeze()

        update = Update(0, channel_post=message)
        assert not handler.check_update(update)

        update = Update(0, edited_channel_post=message)
        assert not handler.check_update(update)

    async def test_all_update_types(self, app: Application, bot: Bot, user1: User) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", self.start_end)], states={}
            )
        )
        message = Message(0, datetime.datetime.now(), self.group, from_user=user1, text="ignore")
        message.set_bot(bot)
        message._unfreeze()
        callback_query = CallbackQuery("0", user1, "", message=message, data="data")
        callback_query.set_bot(bot)
        chosen_inline_result = ChosenInlineResult("0", user1, "query")
        chosen_inline_result.set_bot(bot)
        inline_query = InlineQuery("0", user1, "query", offset="")
        inline_query.set_bot(bot)
        pre_checkout_query = PreCheckoutQuery("0", user1, "USD", 100, "")
        pre_checkout_query.set_bot(bot)
        shipping_query = ShippingQuery("0", user1, "", ShippingAddress("", "", "", "", "", ""))
        shipping_query.set_bot(bot)
        assert not handler.check_update(Update(0, callback_query=callback_query))
        assert not handler.check_update(Update(0, chosen_inline_result=chosen_inline_result))
        assert not handler.check_update(Update(0, inline_query=inline_query))
        assert not handler.check_update(Update(0, message=message))
        assert not handler.check_update(Update(0, pre_checkout_query=pre_checkout_query))
        assert not handler.check_update(Update(0, shipping_query=shipping_query))

    @pytest.mark.parametrize("jq", [True, False])
    async def test_no_running_job_queue_warning(
        self, app: Application, bot: Bot, user1: User, recwarn: WarningsRecorder, jq: bool
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        if not jq:
            app = ApplicationBuilder().token(bot.token).job_queue(None).build()
        app.add_handler(handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.5)
            if jq:
                assert len(recwarn) == 1
            else:
                assert len(recwarn) == 2

            assert str(recwarn[0].message if jq else recwarn[1].message).startswith(
                "Ignoring `conversation_timeout`"
            )
            assert ("is not running" if jq else "No `JobQueue` set up.") in str(recwarn[0].message)
            for warning in recwarn:
                assert warning.category is PTBUserWarning
                assert (
                    Path(warning.filename)
                    == PROJECT_ROOT_PATH
                    / "telegram"
                    / "ext"
                    / "_handlers"
                    / "_conversationhandler"
                    / "conversationhandler.py"
                ), "wrong stacklevel!"
            # now set app.job_queue back to it's original value

    async def test_schedule_job_exception(
        self,
        bot: Bot,
        user1: User,
        monkeypatch: MonkeyPatch,
        caplog: LogCaptureFixture,
    ) -> None:
        def mocked_run_once(*_: Any, **__: Any) -> None:
            raise Exception("job error")

        class DictJB(JobQueue):
            pass

        app = ApplicationBuilder().token(bot.token).job_queue(DictJB()).build()
        monkeypatch.setattr(app.job_queue, "run_once", mocked_run_once)
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=100,
        )
        app.add_handler(handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.start()

            with caplog.at_level(logging.ERROR):
                await app.process_update(Update(update_id=0, message=message))
                await asyncio.sleep(0.5)

            assert len(caplog.records) == 1
            assert caplog.records[0].message == "Failed to schedule timeout."
            assert caplog.records[0].name == "telegram.ext.ConversationHandler"
            assert str(caplog.records[0].exc_info[1]) == "job error"  # type: ignore

            await app.stop()

    @pytest.mark.parametrize(argnames="test_type", argvalues=["none", "exception"])
    async def test_non_blocking_exception_or_none(
        self, app: Application, bot: Bot, user1: User, caplog: LogCaptureFixture, test_type: str
    ) -> None:
        """Here we make sure that when a non-blocking handler raises an
        exception or returns None, the state isn't changed.
        """
        error = Exception("task exception")

        async def conv_entry(_: Any, __: Any) -> int:
            return 1

        async def raise_error(_: Any, __: Any) -> None:
            if test_type == "none":
                return
            raise error

        async def error_handler(_: Any, __: Any) -> None:
            return

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", conv_entry)],
                states={1: [MessageHandler(filters.Text(["error"]), raise_error)]},
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            block=False,
        )
        app.add_handler(handler)
        app.add_error_handler(error_handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        # start the conversation
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.1)
            message.text = "error"
            caplog.clear()
            with caplog.at_level(logging.ERROR):
                await app.process_update(Update(update_id=0, message=message))
                await asyncio.sleep(0.1)
                # This also makes sure that we're still in the same state
                assert handler.check_update(Update(0, message=message))
            if test_type == "exception":
                assert len(caplog.records) == 1
                assert caplog.records[0].name == "telegram.ext.PendingState"
                assert (
                    caplog.records[0].message
                    == "Task function raised exception. Falling back to old state 1"
                )
                assert caplog.records[0].exc_info[1] is None  # type: ignore
            else:
                assert len(caplog.records) == 0

    async def test_non_blocking_entry_point_exception(
        self, app: Application, bot: Bot, user1: User, caplog: LogCaptureFixture
    ) -> None:
        """Here we make sure that when a non-blocking entry point raises an
        exception, the state isn't changed.
        """
        error = Exception("task exception")

        async def raise_error(_: Any, __: Any) -> None:
            raise error

        async def error_handler(_: Any, __: Any) -> None:
            return

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", raise_error, block=False)],
                states={},
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)
        app.add_error_handler(error_handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        # start the conversation
        async with app:
            caplog.clear()
            with caplog.at_level(logging.ERROR):
                await app.process_update(Update(update_id=0, message=message))
                await asyncio.sleep(0.1)
            # This also makes sure that we're still in the same state
            assert handler.check_update(Update(0, message=message))
            assert len(caplog.records) == 1
            assert caplog.records[0].name == "telegram.ext.PendingState"
            assert (
                caplog.records[0].message
                == "An non-blocking entry-point raised an exception. Ending Conversation."
            )
            assert caplog.records[0].exc_info[1] is None  # type: ignore

    async def test_conversation_timeout(self, app: Application, bot: Bot, user1: User) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # Start state machine, then reach timeout
        start_message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        start_message.set_bot(bot)
        brew_message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/brew",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/brew"))
            ],
        )
        brew_message.set_bot(bot)
        pour_coffee_message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/pourCoffee",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/pourCoffee"))
            ],
        )
        pour_coffee_message.set_bot(bot)
        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=start_message))
            assert handler.check_update(Update(0, message=brew_message))
            await asyncio.sleep(0.75)
            assert handler.check_update(Update(0, message=start_message))

            # Start state machine, do something, then reach timeout
            await app.process_update(Update(update_id=1, message=start_message))
            assert handler.check_update(Update(0, message=brew_message))

            await app.process_update(Update(update_id=2, message=brew_message))
            assert handler.check_update(Update(0, message=pour_coffee_message))

            await asyncio.sleep(0.75)
            assert handler.check_update(Update(0, message=start_message))

            await app.stop()

    async def test_timeout_not_triggered_on_conv_end_non_blocking(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        async def timeout(_: Any, __: Any) -> None:
            self.test_flag = True

        self.states.update({ConversationHandler.TIMEOUT: [TypeHandler(Update, timeout)]})
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
            block=False,
        )
        app.add_handler(handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            # start the conversation
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.1)
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=1, message=message))
            await asyncio.sleep(0.1)
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=2, message=message))
            await asyncio.sleep(0.1)
            message.text = "/end"
            message.entities[0].length = len("/end")
            await app.process_update(Update(update_id=3, message=message))
            await asyncio.sleep(1)
            # assert timeout handler didn't get called
            assert self.test_flag is False

    async def test_conversation_timeout_application_handler_stop(
        self, app: Application, bot: Bot, user1: User, recwarn: WarningsRecorder
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )

        async def timeout(_: Any, __: Any) -> None:
            raise ApplicationHandlerStop

        self.states.update({ConversationHandler.TIMEOUT: [TypeHandler(Update, timeout)]})
        app.add_handler(handler)

        # Start state machine, then reach timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            text="/start",
            from_user=user1,
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        brew_message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/brew",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/brew"))
            ],
        )
        brew_message.set_bot(bot)

        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=message))
            # Make sure that we're in the next state
            assert handler.check_update(Update(0, message=brew_message))
            await app.process_update(Update(0, message=brew_message))
            await asyncio.sleep(0.9)
            # Check that conversation has ended by checking that the start messages is accepted
            # again
            assert handler.check_update(Update(0, message=message))
            assert len(recwarn) == 1
            assert str(recwarn[0].message).startswith("ApplicationHandlerStop in TIMEOUT")
            assert recwarn[0].category is PTBUserWarning
            assert (
                Path(recwarn[0].filename)
                == PROJECT_ROOT_PATH / "telegram" / "ext" / "_jobqueue.py"
            ), "wrong stacklevel!"

            await app.stop()

    async def test_conversation_handler_timeout_update_and_context(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        context = None

        async def start_callback(u: Update, c: ContextTypes.DEFAULT_TYPE) -> None:
            nonlocal context, self
            context = c
            return await self.start(u, c)

        # Start state machine, then reach timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        update = Update(update_id=0, message=message)

        async def timeout_callback(u: Update, c: ContextTypes.DEFAULT_TYPE) -> None:
            nonlocal update, context
            assert u is update
            assert c is context

            self.is_timeout = (u is update) and (c is context)

        states = self.states
        timeout_handler = CommandHandler("start", timeout_callback)
        states.update({ConversationHandler.TIMEOUT: [timeout_handler]})
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", start_callback)],
                states=states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        async with app:
            await app.start()

            await app.process_update(update)
            await asyncio.sleep(0.9)
            # check that the conversation has ended by checking that the start message is accepted
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            await app.stop()

    @pytest.mark.flaky(3, 1)
    async def test_conversation_timeout_keeps_extending(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # Start state machine, wait, do something, verify the timeout is extended.
        # t=0 /start (timeout=.5)
        # t=.35 /brew (timeout=.85)
        # t=.5 original timeout
        # t=.6 /pourCoffee (timeout=1.1)
        # t=.85 second timeout
        # t=1.1 actual timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            assert handler.check_update(Update(0, message=message))
            await asyncio.sleep(0.35)  # t=.35
            assert handler.check_update(Update(0, message=message))
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            assert handler.check_update(Update(0, message=message))
            await asyncio.sleep(0.25)  # t=.6
            assert handler.check_update(Update(0, message=message))
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            assert handler.check_update(Update(0, message=message))
            await asyncio.sleep(0.4)  # t=1.0
            assert handler.check_update(Update(0, message=message))
            await asyncio.sleep(0.3)  # t=1.3
            assert not handler.check_update(Update(0, message=message))
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))

            await app.stop()

    async def test_conversation_timeout_two_users(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # Start state machine, do something as second user, then reach timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            assert handler.check_update(Update(0, message=message))
            message.from_user = user2
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/start"
            message.entities[0].length = len("/start")
            # Make sure that user2s conversation has not yet started
            assert handler.check_update(Update(0, message=message))
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            assert handler.check_update(Update(0, message=message))
            await asyncio.sleep(0.7)
            # check that both conversations have ended by checking that the start message is
            # accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            message.from_user = user1
            assert handler.check_update(Update(0, message=message))
            message.from_user = user2
            assert handler.check_update(Update(0, message=message))

            await app.stop()

    async def test_conversation_handler_timeout_state(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        states = self.states
        states.update(
            {
                ConversationHandler.TIMEOUT: [
                    CommandHandler("brew", self.passout),
                    MessageHandler(~filters.Regex("oding"), self.passout2),
                ]
            }
        )
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # CommandHandler timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            # MessageHandler timeout
            self.is_timeout = False
            message.text = "/start"
            message.entities[0].length = len("/start")
            await app.process_update(Update(update_id=1, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            # Timeout but no valid handler
            self.is_timeout = False
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert not self.is_timeout

            await app.stop()

    async def test_conversation_handler_timeout_state_context(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        states = self.states
        states.update(
            {
                ConversationHandler.TIMEOUT: [
                    CommandHandler("brew", self.passout_context),
                    MessageHandler(~filters.Regex("oding"), self.passout2_context),
                ]
            }
        )
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # CommandHandler timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.start()

            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            # MessageHandler timeout
            self.is_timeout = False
            message.text = "/start"
            message.entities[0].length = len("/start")
            await app.process_update(Update(update_id=1, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            # Timeout but no valid handler
            self.is_timeout = False
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.7)
            # check that conversation has ended by checking that start cmd is accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert not self.is_timeout

            await app.stop()

    async def test_conversation_timeout_cancel_conflict(
        self, app: Application, bot: Bot, user1: User
    ) -> None:
        # Start state machine, wait half the timeout,
        # then call a callback that takes more than the timeout
        # t=0 /start (timeout=.5)
        # t=.25 /slowbrew (sleep .5)
        # |  t=.5 original timeout (should not execute)
        # |  t=.75 /slowbrew returns (timeout=1.25)
        # t=1.25 timeout

        async def slowbrew(_: Any, __: Any) -> None:
            await asyncio.sleep(0.25)
            # Let's give to the original timeout a chance to execute
            await asyncio.sleep(0.25)
            # By returning None we do not override the conversation state so
            # we can see if the timeout has been executed

        states = self.states
        states[self.THIRSTY].append(CommandHandler("slowbrew", slowbrew))
        states.update({ConversationHandler.TIMEOUT: [MessageHandler(None, self.passout2)]})

        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(handler)

        # CommandHandler timeout
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()

        async with app:
            await app.start()
            await app.process_update(Update(update_id=0, message=message))
            await asyncio.sleep(0.25)
            message.text = "/slowbrew"
            message.entities[0].length = len("/slowbrew")
            await app.process_update(Update(update_id=0, message=message))
            # Check that conversation has not ended by checking that start cmd is not accepted
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert not handler.check_update(Update(0, message=message))
            assert not self.is_timeout

            await asyncio.sleep(0.7)
            # Check that conversation has ended by checking that start cmd is accepted again
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert self.is_timeout

            await app.stop()

    async def test_nested_conversation_handler(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        self.nested_states[self.DRINKING] = [
            ConversationHandler(
                conversation_states=ConversationStates(
                    entry_points=self.drinking_entry_points,
                    states=self.drinking_states,
                    fallbacks=self.drinking_fallbacks,
                    map_to_parent=self.drinking_map_to_parent,
                )
            )
        ]
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.nested_states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )
        app.add_handler(handler)

        # User one, starts the state machine.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            from_user=user1,
            text="/start",
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            # entry point
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY

            # The user is thirsty and wants to brew coffee. (state THIRSTY)
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING

            # Lets pour some coffee. (state BREWING)
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING

            # The user is holding the cup (nested state DRINKING entry point)
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING

            # The user is sipping coffee (nested state HOLDING)
            message.text = "/sip"
            message.entities[0].length = len("/sip")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.SIPPING

            # The user is swallowing (nested state SIPPING)
            message.text = "/swallow"
            message.entities[0].length = len("/swallow")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.SWALLOWING

            # The user is holding the cup again (nested state SWALLOWING)
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING

            # The user wants to replenish the coffee supply (nested state HOLDING, but fallback)
            message.text = "/replenish"
            message.entities[0].length = len("/replenish")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.REPLENISHING
            # State is mapping us to the Parent state BREWING
            # check that we're in the right state now by checking that the update is accepted
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            assert handler.check_update(Update(0, message=message))

            # The user wants to drink their coffee again)
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING

            # The user is now ready to start coding
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.CODING

            # The user decides it's time to drink again
            message.text = "/drinkMore"
            message.entities[0].length = len("/drinkMore")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING

            # The user is holding their cup
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING

            # The user wants to end with the drinking and go back to coding
            message.text = "/end"
            message.entities[0].length = len("/end")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.END
            # check that we're in the right state now by checking that the update is accepted
            message.text = "/drinkMore"
            message.entities[0].length = len("/drinkMore")
            assert handler.check_update(Update(0, message=message))

            # The user wants to drink once more
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING

            # The user wants to stop altogether
            message.text = "/stop"
            message.entities[0].length = len("/stop")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.STOPPING
            # check that the conversation has ended by checking that the start cmd is accepted
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))

    async def test_nested_conversation_application_handler_stop(
        self, app: Application, bot: Bot, user1: User, user2: User
    ) -> None:
        self.nested_states[self.DRINKING] = [
            ConversationHandler(
                conversation_states=ConversationStates(
                    entry_points=self.drinking_entry_points,
                    states=self.drinking_states,
                    fallbacks=self.drinking_fallbacks,
                    map_to_parent=self.drinking_map_to_parent,
                )
            )
        ]
        handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.nested_states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            )
        )

        async def callback(_: Any, __: Any) -> None:
            self.test_flag = True

        app.add_handler(handler)
        app.add_handler(TypeHandler(Update, callback), group=1)
        self.raise_app_handler_stop = True

        # User one, starts the state machine.
        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            text="/start",
            from_user=user1,
            entities=[
                MessageEntity(type=MessageEntity.BOT_COMMAND, offset=0, length=len("/start"))
            ],
        )
        message.set_bot(bot)
        message._unfreeze()
        message.entities[0]._unfreeze()
        async with app:
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.THIRSTY
            assert not self.test_flag

            # The user is thirsty and wants to brew coffee.
            message.text = "/brew"
            message.entities[0].length = len("/brew")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.BREWING
            assert not self.test_flag

            # Lets pour some coffee.
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING
            assert not self.test_flag

            # The user is holding the cup
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING
            assert not self.test_flag

            # The user is sipping coffee
            message.text = "/sip"
            message.entities[0].length = len("/sip")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.SIPPING
            assert not self.test_flag

            # The user is swallowing
            message.text = "/swallow"
            message.entities[0].length = len("/swallow")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.SWALLOWING
            assert not self.test_flag

            # The user is holding the cup again
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING
            assert not self.test_flag

            # The user wants to replenish the coffee supply
            message.text = "/replenish"
            message.entities[0].length = len("/replenish")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.REPLENISHING
            # check that we're in the right state now by checking that the update is accepted
            message.text = "/pourCoffee"
            message.entities[0].length = len("/pourCoffee")
            assert handler.check_update(Update(0, message=message))
            assert not self.test_flag

            # The user wants to drink their coffee again
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING
            assert not self.test_flag

            # The user is now ready to start coding
            message.text = "/startCoding"
            message.entities[0].length = len("/startCoding")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.CODING
            assert not self.test_flag

            # The user decides it's time to drink again
            message.text = "/drinkMore"
            message.entities[0].length = len("/drinkMore")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING
            assert not self.test_flag

            # The user is holding their cup
            message.text = "/hold"
            message.entities[0].length = len("/hold")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.HOLDING
            assert not self.test_flag

            # The user wants to end with the drinking and go back to coding
            message.text = "/end"
            message.entities[0].length = len("/end")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.END
            # check that we're in the right state now by checking that the update is accepted
            message.text = "/drinkMore"
            message.entities[0].length = len("/drinkMore")
            assert handler.check_update(Update(0, message=message))
            assert not self.test_flag

            # The user wants to drink once more
            message.text = "/drinkMore"
            message.entities[0].length = len("/drinkMore")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.DRINKING
            assert not self.test_flag

            # The user wants to stop altogether
            message.text = "/stop"
            message.entities[0].length = len("/stop")
            await app.process_update(Update(update_id=0, message=message))
            assert self.current_state[user1.id] == self.STOPPING
            # check that the conv has ended by checking that the start cmd is accepted
            message.text = "/start"
            message.entities[0].length = len("/start")
            assert handler.check_update(Update(0, message=message))
            assert not self.test_flag

    @pytest.mark.parametrize("callback_raises", [True, False])
    async def test_timeout_non_block(
        self, app: Application, user1: User, callback_raises: bool
    ) -> None:
        event = asyncio.Event()

        async def callback(_: Any, __: Any) -> int:
            await event.wait()
            if callback_raises:
                raise RuntimeError
            return 1

        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[MessageHandler(filters.ALL, callback=callback, block=False)],
                states={
                    ConversationHandler.TIMEOUT: [TypeHandler(Update, self.passout2)],
                    1: [TypeHandler(Update, callback)],
                },
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(conv_handler)

        async with app:
            await app.start()

            message = Message(
                0,
                datetime.datetime.now(),
                self.group,
                text="/start",
                from_user=user1,
            )
            assert conv_handler.check_update(Update(0, message=message))
            await app.process_update(Update(0, message=message))
            await asyncio.sleep(0.7)
            tasks = asyncio.all_tasks()
            assert any(":handle_update:non_blocking_cb" in t.get_name() for t in tasks)
            assert not self.is_timeout
            event.set()
            await asyncio.sleep(0.7)
            assert self.is_timeout == (not callback_raises)

            await app.stop()

    async def test_no_timeout_on_end(self, app: Application, user1: User) -> None:
        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[MessageHandler(filters.ALL, callback=self.start_end)],
                states={ConversationHandler.TIMEOUT: [TypeHandler(Update, self.passout2)]},
            ),
            conversation_timeout=0.5,
        )
        app.add_handler(conv_handler)

        async with app:
            await app.start()

            message = Message(
                0,
                datetime.datetime.now(),
                self.group,
                text="/start",
                from_user=user1,
            )
            assert conv_handler.check_update(Update(0, message=message))
            await app.process_update(Update(0, message=message))
            await asyncio.sleep(0.7)
            assert not self.is_timeout

            await app.stop()

    async def test_conversation_handler_block_dont_override(self, app: Application) -> None:
        """This just makes sure that we don't change any attributes of the handlers of the conv"""
        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=self.entry_points,
                states=self.states,
                pre_fallbacks=self.pre_fallbacks,
                fallbacks=self.fallbacks,
            ),
            block=False,
        )

        all_handlers = (
            conv_handler.available_states.entry_points + conv_handler.available_states.fallbacks
        )
        for state_handlers in conv_handler.available_states.states.values():
            all_handlers += state_handlers

        for handler in all_handlers:
            assert handler.block

        conv_handler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[CommandHandler("start", self.start_end, block=False)],
                states={1: [CommandHandler("start", self.start_end, block=False)]},
                fallbacks=[CommandHandler("start", self.start_end, block=False)],
            ),
            block=True,
        )

        all_handlers = (
            conv_handler.available_states.entry_points + conv_handler.available_states.fallbacks
        )
        for state_handlers in conv_handler.available_states.states.values():
            all_handlers += state_handlers

        for handler in all_handlers:
            assert handler.block is False

    @pytest.mark.parametrize("default_block", [True, False, None])
    @pytest.mark.parametrize("ch_block", [True, False, None])
    @pytest.mark.parametrize("handler_block", [True, False, None])
    @pytest.mark.parametrize("ext_bot", [True, False], ids=["ExtBot", "Bot"])
    async def test_blocking_resolution_order(
        self,
        bot_info: dict,
        default_block: bool,
        ch_block: bool,
        handler_block: bool,
        ext_bot: bool,
    ) -> None:
        event = asyncio.Event()

        async def callback(_: Any, __: Any) -> int:
            await event.wait()
            event.clear()
            self.test_flag = True
            return 1

        if handler_block is not None:
            handler = CommandHandler("start", callback=callback, block=handler_block)
            fallback = MessageHandler(filters.ALL, callback, block=handler_block)
        else:
            handler = CommandHandler("start", callback=callback)
            fallback = MessageHandler(filters.ALL, callback, block=handler_block)

        defaults = Defaults(block=default_block) if default_block is not None else None

        if ch_block is not None:
            conv_handler: ConversationHandler = ConversationHandler(
                conversation_states=ConversationStates(
                    entry_points=[handler],
                    states={1: [handler]},
                    fallbacks=[fallback],
                ),
                block=ch_block,
            )
        else:
            conv_handler = ConversationHandler(
                conversation_states=ConversationStates(
                    entry_points=[handler],
                    states={1: [handler]},
                    fallbacks=[fallback],
                )
            )

        bot = make_bot(bot_info, defaults=defaults) if ext_bot else PytestBot(bot_info["token"])
        app = ApplicationBuilder().bot(bot).build()
        app.add_handler(conv_handler)

        async with app:
            start_message = make_command_message("/start")
            start_message.set_bot(bot)
            fallback_message = make_command_message("/fallback")
            fallback_message.set_bot(bot)

            # This loop makes sure that we test all of entry points, states handler & fallbacks
            for message in [start_message, fallback_message]:
                process_update_task = asyncio.create_task(
                    app.process_update(Update(0, message=message))
                )
                if (
                    # resolution order is handler_block -> ch_block -> default_block
                    # setting block=True/False on a lower priority setting may only have an effect
                    # if it wasn't set for the higher priority settings
                    (handler_block is False)
                    or ((handler_block is None) and (ch_block is False))
                    or (
                        (handler_block is None)
                        and (ch_block is None)
                        and ext_bot
                        and (default_block is False)
                    )
                ):
                    # check that the handler was called non-blocking by checking that
                    # `process_update` has finished
                    await asyncio.sleep(0.01)
                    assert process_update_task.done()
                else:
                    # the opposite
                    assert not process_update_task.done()

                # In any case, the callback must not have finished
                assert not self.test_flag

                # After setting the event, the callback must have finished and in the blocking
                # case this leads to `process_update` finishing.
                event.set()
                await asyncio.sleep(0.01)
                assert process_update_task.done()
                assert self.test_flag
                self.test_flag = False

    async def test_waiting_state(self, app: Application, user1: User) -> None:
        event = asyncio.Event()

        async def callback_1(_: Any, __: Any) -> None:
            self.test_flag = 1

        async def callback_2(_: Any, __: Any) -> None:
            self.test_flag = 2

        async def callback_3(_: Any, __: Any) -> None:
            self.test_flag = 3

        async def blocking(_: Any, __: Any) -> int:
            await event.wait()
            return 1

        conv_handler: ConversationHandler = ConversationHandler(
            conversation_states=ConversationStates(
                entry_points=[MessageHandler(filters.ALL, callback=blocking, block=False)],
                states={
                    ConversationHandler.WAITING: [
                        MessageHandler(filters.Regex("1"), callback_1),
                        MessageHandler(filters.Regex("2"), callback_2),
                    ],
                    1: [MessageHandler(filters.Regex("2"), callback_3)],
                },
            ),
        )
        app.add_handler(conv_handler)

        message = Message(
            0,
            datetime.datetime.now(),
            self.group,
            text="/start",
            from_user=user1,
        )
        message._unfreeze()

        async with app:
            await app.process_update(Update(0, message=message))
            assert not self.test_flag
            message.text = "1"
            await app.process_update(Update(0, message=message))
            assert self.test_flag == 1
            message.text = "2"
            await app.process_update(Update(0, message=message))
            assert self.test_flag == 2
            event.set()
            await asyncio.sleep(0.05)
            self.test_flag = None
            await app.process_update(Update(0, message=message))
            assert self.test_flag == 3

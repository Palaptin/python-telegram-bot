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

import pytest
from _pytest.recwarn import WarningsRecorder

from telegram.ext import ConversationHandler, ConversationStates
from tests.auxil.slots import mro_slots


class TestConversationStates:

    @staticmethod
    def get_conversation_handler() -> ConversationHandler:
        return ConversationHandler(
            conversation_states=ConversationStates(entry_points=[], states={})
        )

    def test_slot_behaviour(self) -> None:
        conversation_states: ConversationStates = ConversationStates(entry_points=[], states={})
        for attr in conversation_states.__slots__:
            assert getattr(conversation_states, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(conversation_states)) == len(
            set(mro_slots(conversation_states))
        ), "duplicate slot"

    def test_init(self, recwarn: WarningsRecorder) -> None:

        entry_points: list = []
        states: dict = {ConversationHandler.END: self.get_conversation_handler()}
        state_entry_handlers: dict = {}
        pre_fallbacks: list = []
        fallbacks: list = []
        map_to_parent: dict = {}
        allow_reentry = True

        recwarn.clear()
        cs: ConversationStates = ConversationStates(
            entry_points=entry_points,
            states=states,
            state_entry_handlers=state_entry_handlers,
            pre_fallbacks=pre_fallbacks,
            fallbacks=fallbacks,
            map_to_parent=map_to_parent,
            allow_reentry=allow_reentry,
        )
        assert str(recwarn[0].message) == (
            "The END state (-1) is reserved and shouldn't be used as a state name in the "
            "`ConversationHandler`."
        )
        assert cs.entry_points is entry_points
        assert cs.states is states
        assert cs.state_entry_handlers is state_entry_handlers
        assert cs.pre_fallbacks is pre_fallbacks
        assert cs.fallbacks is fallbacks
        assert cs.map_to_parent is map_to_parent
        assert cs.allow_reentry is allow_reentry

    def test_init_with_default_params(self) -> None:
        entry_points: list = []
        states: dict = {}

        cs: ConversationStates = ConversationStates(entry_points=entry_points, states=states)

        assert cs.entry_points is entry_points
        assert cs.states is states
        assert cs.state_entry_handlers == {}
        assert cs.pre_fallbacks == []
        assert cs.fallbacks == []
        assert cs.map_to_parent is None
        assert cs.allow_reentry is False

    @pytest.mark.parametrize(
        "attr",
        [
            "entry_points",
            "pre_fallbacks",
            "states",
            "state_entry_handlers",
            "fallbacks",
            "map_to_parent",
            "allow_reentry",
        ],
    )
    def test_immutable(self, attr: str) -> None:
        cs: ConversationStates = ConversationStates(entry_points=[], states={})
        print(attr)
        with pytest.raises(AttributeError, match=f"You can not assign a new value to {attr}"):
            setattr(cs, attr, True)

    def test_get_all_handlers(self) -> None:

        entry_points: list = [self.get_conversation_handler()]
        states: dict = {1: [self.get_conversation_handler()]}
        state_entry_handlers: dict = {1: [self.get_conversation_handler()]}
        pre_fallbacks: list = [self.get_conversation_handler()]
        fallbacks: list = [self.get_conversation_handler()]
        allow_reentry = True

        cs: ConversationStates = ConversationStates(
            entry_points=entry_points,
            states=states,
            state_entry_handlers=state_entry_handlers,
            pre_fallbacks=pre_fallbacks,
            fallbacks=fallbacks,
            allow_reentry=allow_reentry,
        )

        assert len(cs.get_all_handlers()) == 5

    def test_get_child_conversations(self) -> None:

        entry_points: list = [self.get_conversation_handler()]
        states: dict = {1: [self.get_conversation_handler()]}
        state_entry_handlers: dict = {1: [self.get_conversation_handler()]}
        pre_fallbacks: list = [self.get_conversation_handler()]
        fallbacks: list = [self.get_conversation_handler()]
        allow_reentry = True

        cs: ConversationStates = ConversationStates(
            entry_points=entry_points,
            states=states,
            state_entry_handlers=state_entry_handlers,
            pre_fallbacks=pre_fallbacks,
            fallbacks=fallbacks,
            allow_reentry=allow_reentry,
        )

        assert len(cs.get_child_conversations()) == 5

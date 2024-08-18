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
from unittest.mock import Mock

from telegram import Update
from telegram.ext import ConversationData
from tests.auxil.slots import mro_slots


class TestConversationData:

    key = (1, 2)
    state = 3
    timeout = 4
    update = Update(update_id=1)
    conversation_context_data = {1}

    cd: ConversationData = ConversationData(
        key=key,
        state=state,
        timeout=timeout,
        update=update,
        conversation_context_data=conversation_context_data,
    )

    def test_slot_behaviour(self) -> None:
        conversation_data: ConversationData = ConversationData((1, 1), None)
        for attr in conversation_data.__slots__:
            assert getattr(conversation_data, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(conversation_data)) == len(
            set(mro_slots(conversation_data))
        ), "duplicate slot"

    def test_init(self) -> None:

        assert self.cd.key is self.key
        assert self.cd.state == self.state
        assert self.cd.timeout == self.timeout
        assert self.cd.update == self.update
        assert self.cd.conversation_context_data is self.conversation_context_data

    def test_init_default_values(self) -> None:

        key = (1, 2)
        state = 3

        cd: ConversationData = ConversationData(key=key, state=state)

        assert cd.key is key
        assert cd.state == state
        assert cd.timeout is None
        assert cd.update is None
        assert cd.conversation_context_data == {}

    def test_copy(self) -> None:

        copy = self.cd.copy()
        assert copy == self.cd
        assert copy is not self.cd

    def test_to_dict(self) -> None:

        # test all values are present in the dict
        assert self.cd.to_dict() == {
            "conversation_context_data": {1},
            "key": (1, 2),
            "state": 3,
            "timeout": 4,
            "update": {"update_id": 1},
        }

        # test empty values are also present
        cd_minimal: ConversationData = ConversationData(key=(1, 2), state=3)
        assert cd_minimal.to_dict() == {
            "conversation_context_data": {},
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": None,
        }

        # test a custom update will be present in the dict
        update = object
        cd_update_no_to_dict: ConversationData = ConversationData(
            key=(1, 2), state=3, update=update
        )
        assert cd_update_no_to_dict.to_dict() == {
            "conversation_context_data": {},
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": update,
        }

        # test a customs update to_dict return value will be present in the dict
        dummy_update: Mock = Mock()
        dummy_update.to_dict.return_value = 42
        cd_custom_update: ConversationData = ConversationData(
            key=(1, 2), state=3, update=dummy_update
        )
        assert cd_custom_update.to_dict() == {
            "conversation_context_data": {},
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": 42,
        }

        # test conversation_context_data can be something else than dict
        conversation_context_data = object
        cd_custom_cod: ConversationData = ConversationData(
            key=(1, 2), state=3, conversation_context_data=conversation_context_data
        )
        assert cd_custom_cod.to_dict() == {
            "conversation_context_data": conversation_context_data,
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": None,
        }

    def test_equality(self) -> None:
        conv_data1: ConversationData = ConversationData(
            (1, 1), 1, 1, 1, conversation_context_data={}
        )
        conv_data2: ConversationData = ConversationData(
            (1, 1), 1, 1, 1, conversation_context_data={}
        )
        conv_data3: ConversationData = ConversationData(
            (1, 2), 2, 2, 2, conversation_context_data=[]
        )
        object_4 = object
        assert conv_data1 == conv_data2
        assert conv_data1 != conv_data3
        assert conv_data1 != object_4

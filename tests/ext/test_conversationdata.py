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

import pytest

from telegram import Update
from telegram.ext import ConversationData
from tests.auxil.slots import mro_slots


@pytest.fixture
def conversation_data() -> ConversationData:
    return ConversationData(
        key=(1, 2), state=3, timeout=4, update=Update(update_id=1), conversation_context_data={1}
    )


class TestConversationDataWithoutRequest:

    def test_slot_behaviour(self) -> None:
        conversation_data: ConversationData = ConversationData((1, 1), None)
        for attr in conversation_data.__slots__:
            assert getattr(conversation_data, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(conversation_data)) == len(
            set(mro_slots(conversation_data))
        ), "duplicate slot"

    def test_init(self, conversation_data: ConversationData) -> None:
        assert conversation_data.key == (1, 2)
        assert conversation_data.state == 3
        assert conversation_data.timeout == 4
        assert conversation_data.update == Update(update_id=1)
        assert conversation_data.conversation_context_data == {1}

    def test_init_default_values(self) -> None:
        cd: ConversationData = ConversationData(key=(1, 2), state=3)
        assert cd.key == (1, 2)
        assert cd.state == 3
        assert cd.timeout is None
        assert cd.update is None
        assert (
            cd.conversation_context_data == {}
        ), "Default conversation_context_data should be an empty dictionary"

    def test_copy(self, conversation_data: ConversationData) -> None:
        """Tests if a copy is not the same object, but equal to the original object"""
        copy = conversation_data.copy()
        assert copy == conversation_data, "The copied instance should be equal to the original"
        assert copy is not conversation_data, "The copied instance should not be the same object"

    def test_to_dict(self, conversation_data: ConversationData) -> None:
        """Tests the conversion of the ConversationData into a dictionary."""

        # Test all values are present in the dict
        dict_rep = conversation_data.to_dict()
        assert dict_rep == {
            "key": (1, 2),
            "state": 3,
            "timeout": 4,
            "update": {"update_id": 1},
            "conversation_context_data": {1},
        }

        # Test with minimal values
        cd_minimal: ConversationData = ConversationData(key=(1, 2), state=3)
        assert cd_minimal.to_dict() == {
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": None,
            "conversation_context_data": {},
        }

        # Test a custom update will be present in the dict if it has no to_dict method
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
        dummy_update = Mock()
        dummy_update.to_dict.return_value = 42
        cd_with_mock_update: ConversationData = ConversationData(
            key=(1, 2), state=3, update=dummy_update
        )

        assert cd_with_mock_update.to_dict() == {
            "key": (1, 2),
            "state": 3,
            "timeout": None,
            "update": 42,  # The expected mock result from .to_dict of the update object
            "conversation_context_data": {},
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

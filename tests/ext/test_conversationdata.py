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

        assert self.cd.to_dict() == {
            "conversation_context_data": {1},
            "key": (1, 2),
            "state": 3,
            "timeout": 4,
            "update": {"update_id": 1},
        }

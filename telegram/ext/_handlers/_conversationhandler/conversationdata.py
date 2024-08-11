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
"""This module contains the ConversationData."""
import datetime
import json
from dataclasses import dataclass
from typing import Generic, Optional, Union

from telegram import Bot, Update
from telegram.ext._utils.types import COD, ConversationKey


@dataclass
class ConversationData(Generic[COD]):
    """Used as a datastore for persistence, as well as for internal state handling.

    .. versionadded:: NEXT.VERSION
    Args:
        key (:obj:`ConversationKey`): The key of the corresponding conversation.
        state (:obj:`Object`): The state in which the corresponding conversation is currently.
        timeout (:obj:`float` | :obj:`datetime.timedelta`, optional): Next timeout of the
            corresponding timeout job.
        update (_obj:`Update`, optional): The :obj:`update`.
                conversation_context_data :obj:`ConversationData.conversation_data`: An object that
                can be used to keep any data in. For each update from the same conversation it will
                be the same


    Attributes:
        key (:obj:`ConversationKey`): The key of the corresponding conversation.
        state (:obj:`Object`): The state in which the corresponding conversation is currently.
        timeout (:obj:`float` | :obj:`datetime.timedelta`): Next timeout of the
            corresponding timeout job.
        json_update (_obj:`str`): The update in json form for the
            corresponding timeout job.
        conversation_context_data (:obj:`ConversationData.conversation_data`): An object that can
        be used to keep any data in. For each update from the same conversation it will be the same

    """

    __slots__ = ("conversation_context_data", "json_update", "key", "state", "timeout")

    def __init__(
        self,
        key: ConversationKey,
        state: object,
        timeout: Optional[Union[float, datetime.timedelta]] = None,
        update: Optional[Update] = None,
        conversation_context_data: Optional[COD] = None,
    ):
        if conversation_context_data is None:
            self.conversation_context_data: COD = {}  # type: ignore
        else:
            self.conversation_context_data = conversation_context_data

        self.key: ConversationKey = key
        self.state: object = state
        self.timeout: Optional[Union[float, datetime.timedelta]] = timeout
        self.json_update: str = ""
        self.set_update(update)

    def set_update(self, update: Optional[Update]) -> None:
        """set the attribute json_update from an :obj:`Update`"""
        self.json_update = update.to_json() if update is not None else ""

    def update_as_object(self, bot: Bot) -> Optional[Update]:
        """
        Creates an update from the :attr:`json_update`.

        Args:
            bot (:class:`telegram.Bot`): The bot.

        Returns:the Update created from :attr:`json_update`.

        """
        return Update.de_json(json.loads(self.json_update), bot) if self.json_update else None

    def copy(self) -> "ConversationData":
        """
        Creates a copy of itself.

        Returns: a copy of itself.

        """
        copy: ConversationData = ConversationData(
            key=self.key,
            state=self.state,
            timeout=self.timeout,
            conversation_context_data=self.conversation_context_data,
        )
        copy.json_update = self.json_update
        return copy

    def to_dict(self) -> dict:
        """
        Helper method to transform the internal data to an :obj:`dict`.

        Returns: a dict with the keys: key, state, timeout, update.

        """
        return {
            "key": self.key,
            "state": self.state,
            "timeout": self.timeout,
            "update": self.json_update if self.json_update != "" else None,
            "conversation_context_data": self.conversation_context_data,
        }

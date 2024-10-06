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
from dataclasses import dataclass
from typing import Any, Generic, Optional, Union

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
        update (_obj:`object`): The update for the corresponding timeout job.
        conversation_context_data (:obj:`ConversationData.conversation_data`): An object that can
        be used to keep any data in. For each update from the same conversation it will be the same

    """

    __slots__ = ("conversation_context_data", "key", "state", "timeout", "update")

    def __init__(
        self,
        key: ConversationKey,
        state: object,
        timeout: Optional[Union[float, datetime.timedelta]] = None,
        update: Optional[object] = None,
        conversation_context_data: Optional[COD] = None,
    ):
        if conversation_context_data is None:
            self.conversation_context_data: COD = {}  # type: ignore
        else:
            self.conversation_context_data = conversation_context_data
        self.key: ConversationKey = key
        self.state: object = state
        self.timeout: Optional[Union[float, datetime.timedelta]] = timeout
        self.update = update

    def __eq__(self, other: object) -> bool:
        """Compares this object with :paramref:`other` in terms of equality.
        If this object and :paramref:`other` are `not` objects of the same class,
        this comparison will fall back to Python's default implementation of :meth:`object.__eq__`.
        The objects are considered to be equal, if all the attributes are equal.

        Args:
            other (:obj:`object`): The object to compare with.

        Returns:
            :obj:`bool`

        """
        if not isinstance(other, self.__class__):
            return super().__eq__(other)

        # get all values dynamically
        self_values = [self.__getattribute__(attr) for attr in self.__slots__]
        others_values = [other.__getattribute__(attr) for attr in other.__slots__]
        return self_values == others_values

    def copy(self) -> "ConversationData":
        """
        Creates a copy of itself.

        Returns: a copy of itself.

        """
        copy: ConversationData = ConversationData(
            key=self.key,
            state=self.state,
            timeout=self.timeout,
            update=self.update,
            conversation_context_data=self.conversation_context_data,
        )
        return copy

    def to_dict(self) -> dict:
        """
        Helper method to transform the internal data to an :obj:`dict`.

        Returns: a dict with the keys: key, state, timeout, update and conversation_context_data.

        """

        if hasattr(self.update, "to_dict"):
            dict_update: Any = self.update.to_dict()
        else:
            dict_update = self.update
        return {
            "key": self.key,
            "state": self.state,
            "timeout": self.timeout,
            "update": dict_update,
            "conversation_context_data": self.conversation_context_data,
        }

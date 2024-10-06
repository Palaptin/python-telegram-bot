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
"""This module contains the ConversationHandlerKey."""
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any

from telegram.ext._utils.types import CCT, ConversationKey

if TYPE_CHECKING:
    from telegram.ext import BaseHandler


class ConversationHandlerKey(ABC):
    """Interface class for creating keys for the ConversationHandler.
    These keys are used to match Updates to a conversation.
    Subclass this object for different implementations of key generation.

    .. versionadded:: NEXT.VERSION
    """

    @abstractmethod
    def from_update(self, update: object) -> ConversationKey:
        """Returns the ConversationHandlerKey for the update.
        If more than one key can be built for the update, the implementation may either choose
        one or raise an exception. If the update is not supported, the implementation may return
        NotImplemented in which case no handler of this ConversationHandler will be executed.
        """

    @abstractmethod
    def warn_if_handler_is_invalid(self, handler: "BaseHandler[Any, CCT, object]") -> None:
        """Checks if the Handler is supported with this ConversationHandlerKey. If not, raise a
        Warning. This method will be called on the  initialization of :obj:`ConversationHandler`
        with all added handlers.

        Parameters:
            handler (:obj:`Basehandler`): The handler to check for compatibility."""

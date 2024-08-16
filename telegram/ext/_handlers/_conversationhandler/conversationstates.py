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
"""This module contains the ConversationStates."""
from typing import Dict, Generic, List, NoReturn, Optional, Set

from telegram._utils.warnings import warn
from telegram.ext import ConversationHandler
from telegram.ext._handlers.basehandler import BaseHandler
from telegram.ext._utils.types import CCT


class ConversationStates(Generic[CCT]):
    """
    Used as a datastore for ConversationHandler states. Passed in the
    :paramref:`ConversationHandler.__init__.conversation_states` parameter.
    Args:
        entry_points (List[:class:`telegram.ext.BaseHandler`]): A list of :obj:`BaseHandler`
            objects that
            can trigger the start of the conversation. The first handler whose :meth:`check_update`
            method returns :obj:`True` will be used. If all return :obj:`False`, the update is not
            handled.
        states (Dict[:obj:`object`, List[:class:`telegram.ext.BaseHandler`]]): A :obj:`dict` that
            defines the different states of conversation a user can be in and one or more
            associated :obj:`BaseHandler` objects that should be used in that state. The first
            handler whose :meth:`check_update` method returns :obj:`True` will be used.
        state_entry_handlers (Dict[:obj:`object`, List[:class:`telegram.ext.BaseHandler`]]): A
            :obj:`dict` that defines different :obj:`BaseHandler` objects that should be used by
            entering the corresponding state. The first handler whose :meth:`check_update` method
             returns :obj:`True` will be used. If all return :obj:`False`, the update is not
             handled here.

             .. versionadded:: NEXT.VERSION
        pre_fallbacks (List[:class:`telegram.ext.BaseHandler`], optional): A list of handlers that
            will be checked at first if the user is in a conversation. The first handler which
            :meth:`check_update` method returns :obj:`True` will be used. If all return
            :obj:`False`, the update is not handled here.

            .. versionadded:: NEXT.VERSION
        fallbacks (List[:class:`telegram.ext.BaseHandler`], optional): A list of handlers that
            might be used if the user is in a conversation, but every handler for their current
            state returned :obj:`False` on :meth:`check_update`. The first handler which
            :meth:`check_update` method returns :obj:`True` will be used. If all return
            :obj:`False`, the update is not handled.
        map_to_parent (Dict[:obj:`object`, :obj:`object`], optional): A :obj:`dict` that can be
            used to instruct a child conversation handler to transition into a mapped state on
            its parent conversation handler in place of a specified nested state.
        allow_reentry (:obj:`bool`, optional): If set to :obj:`True`, a user that is currently in a
            conversation can restart the conversation by triggering one of the entry points.
            Default is :obj:`False`.
    """

    __slots__ = (
        "_allow_reentry",
        "_entry_points",
        "_fallbacks",
        "_map_to_parent",
        "_pre_fallbacks",
        "_state_entry_handlers",
        "_states",
    )

    def __init__(
        self,
        entry_points: List[BaseHandler],
        states: Dict[object, List[BaseHandler]],
        state_entry_handlers: Optional[Dict[object, List[BaseHandler]]] = None,
        pre_fallbacks: Optional[List[BaseHandler]] = None,
        fallbacks: Optional[List[BaseHandler]] = None,
        map_to_parent: Optional[Dict[object, object]] = None,
        allow_reentry: bool = False,
    ):
        if pre_fallbacks is None:
            pre_fallbacks = []

        if fallbacks is None:
            fallbacks = []

        if state_entry_handlers is None:
            state_entry_handlers = {}
        self._entry_points: List[BaseHandler] = entry_points
        self._pre_fallbacks: List[BaseHandler] = pre_fallbacks
        self._states: Dict[object, List[BaseHandler]] = states
        self._state_entry_handlers: Dict[object, List[BaseHandler]] = state_entry_handlers
        self._fallbacks: List[BaseHandler] = fallbacks
        self._map_to_parent: Optional[Dict[object, object]] = map_to_parent

        self._allow_reentry: bool = allow_reentry

        for key in states:
            if key == ConversationHandler.END:
                warn(
                    f"The END state ({key!r}) is reserved and shouldn't be used as a state name"
                    f" in the `ConversationHandler`.",
                    stacklevel=2,
                )

    def get_all_handlers(self) -> List[BaseHandler]:
        all_handlers: List[BaseHandler] = []
        all_handlers.extend(self.pre_fallbacks)
        all_handlers.extend(self.entry_points)
        all_handlers.extend(self.fallbacks)

        for state_handlers in self.states.values():
            all_handlers.extend(state_handlers)
        for state_entry_handler in self.state_entry_handlers.values():
            all_handlers.extend(state_entry_handler)
        return all_handlers

    def get_child_conversations(self) -> Set[ConversationHandler]:
        child_conversations: Set[ConversationHandler] = set()
        child_conversations.update(
            handler
            for handler in self.get_all_handlers()
            if isinstance(handler, ConversationHandler)
        )
        return child_conversations

    @property
    def entry_points(self) -> List[BaseHandler]:
        """List[:class:`telegram.ext.BaseHandler`]: A list of :obj:`BaseHandler` objects that can
        trigger the start of the conversation.
        """
        return self._entry_points

    @entry_points.setter
    def entry_points(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to entry_points after initialization."
        )

    @property
    def pre_fallbacks(self) -> List[BaseHandler]:
        """List[:class:`telegram.ext.BaseHandler`]: A list of handlers that will be
        checked before checking the different states.

        .. versionadded:: NEXT.VERSION
        """
        return self._pre_fallbacks

    @pre_fallbacks.setter
    def pre_fallbacks(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to pre_fallbacks after initialization."
        )

    @property
    def states(self) -> Dict[object, List[BaseHandler]]:
        """Dict[:obj:`object`, List[:class:`telegram.ext.BaseHandler`]]: A :obj:`dict` that
        defines the different states of conversation a user can be in and one or more
        associated :obj:`BaseHandler` objects that should be used in that state.
        """
        return self._states

    @states.setter
    def states(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to states after initialization.")

    @property
    def state_entry_handlers(self) -> Dict[object, List[BaseHandler]]:
        """Dict[:obj:`object`, List[:class:`telegram.ext.BaseHandler`]]: A :obj:`dict` that
        defines different entry handlers for the different states of conversation a user can be
        in and one or more associated :obj:`BaseHandler` objects that should be used in that
        state.

        .. versionadded:: NEXT.VERSION
        """
        return self._state_entry_handlers

    @state_entry_handlers.setter
    def state_entry_handlers(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to state_entry_handlers after initialization."
        )

    @property
    def fallbacks(self) -> List[BaseHandler]:
        """List[:class:`telegram.ext.BaseHandler`]: A list of handlers that might be used if
        the user is in a conversation, but every handler for their current state returned
        :obj:`False` on :meth:`check_update`.
        """
        return self._fallbacks

    @fallbacks.setter
    def fallbacks(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to fallbacks after initialization.")

    @property
    def map_to_parent(self) -> Optional[Dict[object, object]]:
        """Dict[:obj:`object`, :obj:`object`]: Optional. A :obj:`dict` that can be
        used to instruct a nested :class:`ConversationHandler` to transition into a mapped state on
        its parent :class:`ConversationHandler` in place of a specified nested state.
        """
        return self._map_to_parent

    @map_to_parent.setter
    def map_to_parent(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to map_to_parent after initialization."
        )

    @property
    def allow_reentry(self) -> bool:
        """:obj:`bool`: Determines if a user can restart a conversation with an entry point."""
        return self._allow_reentry

    @allow_reentry.setter
    def allow_reentry(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to allow_reentry after initialization."
        )

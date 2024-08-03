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
"""This module contains the ConversationHandler."""
import asyncio
import datetime
import json
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import (
    TYPE_CHECKING,
    Any,
    Dict,
    Final,
    Generic,
    List,
    NoReturn,
    Optional,
    Set,
    Tuple,
    Union,
    cast,
)

from telegram import Bot, Update
from telegram._utils.defaultvalue import DEFAULT_TRUE, DefaultValue
from telegram._utils.logging import get_logger
from telegram._utils.repr import build_repr_with_selected_attrs
from telegram._utils.types import DVType
from telegram._utils.warnings import warn
from telegram.ext._application import ApplicationHandlerStop
from telegram.ext._extbot import ExtBot
from telegram.ext._handlers.basehandler import BaseHandler
from telegram.ext._handlers.callbackqueryhandler import CallbackQueryHandler
from telegram.ext._handlers.choseninlineresulthandler import ChosenInlineResultHandler
from telegram.ext._handlers.inlinequeryhandler import InlineQueryHandler
from telegram.ext._handlers.stringcommandhandler import StringCommandHandler
from telegram.ext._handlers.stringregexhandler import StringRegexHandler
from telegram.ext._handlers.typehandler import TypeHandler
from telegram.ext._utils.trackingdict import TrackingDict
from telegram.ext._utils.types import CCT, ConversationDict, ConversationKey

if TYPE_CHECKING:
    from telegram.ext import Application, Job, JobQueue
_CheckUpdateType = Tuple[object, ConversationKey, BaseHandler[Update, CCT], object]

_LOGGER = get_logger(__name__, class_name="ConversationHandler")


@dataclass
class _ConversationTimeoutContext(Generic[CCT]):
    """Used as a datastore for conversation timeouts. Passed in the
    :paramref:`JobQueue.run_once.data` parameter. See :meth:`_trigger_timeout`.
    """

    __slots__ = ("application", "callback_context", "conversation_key", "update")

    conversation_key: ConversationKey
    update: Update
    application: "Application[Any, CCT, Any, Any, Any, JobQueue]"
    callback_context: CCT


@dataclass
class ConversationData:
    """Used as a datastore for persistence, as well as for internal state handling.

    .. versionadded:: NEXT.VERSION
    Args:
        key (:obj:`ConversationKey`): The key of the corresponding conversation.
        state (:obj:`Object`): The state in which the corresponding conversation is currently.
        timeout (:obj:`float` | :obj:`datetime.timedelta`, optional): Next timeout of the
            corresponding timeout job.
        update (_obj:`Update`, optional): The :obj:`update`.

    Attributes:
        key (:obj:`ConversationKey`): The key of the corresponding conversation.
        state (:obj:`Object`): The state in which the corresponding conversation is currently.
        timeout (:obj:`float` | :obj:`datetime.timedelta`): Next timeout of the
            corresponding timeout job.
        json_update (_obj:`str`): The update in json form for the
            corresponding timeout job.

    """

    key: ConversationKey
    state: object
    timeout: Optional[Union[float, datetime.timedelta]]
    json_update: str

    def __init__(
        self,
        key: ConversationKey,
        state: object,
        timeout: Optional[Union[float, datetime.timedelta]] = None,
        update: Optional[Update] = None,
    ):
        self.key = key
        self.state = state
        self.timeout = timeout
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
        copy = ConversationData(self.key, self.state, self.timeout)
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
        }


@dataclass
class PendingState:
    """Thin wrapper around :class:`asyncio.Task` to handle block=False handlers. Note that this is
    a public class of this module, since :meth:`Application.update_persistence` needs to access it.
    It's still hidden from users, since this module itself is private.
    """

    __slots__ = (
        "application",
        "block",
        "context",
        "conv_handler",
        "conversation_data",
        "handler",
        "old_state",
        "task",
        "update",
    )

    task: asyncio.Task
    old_state: object

    def __init__(
        self,
        old_state: object,
        task: asyncio.Task,
        conv_handler: "ConversationHandler",
        conversation_data: ConversationData,
        handler: Optional[BaseHandler],
        update: Update,
        context: CCT,
        application: "Application",
        block: DVType[bool],
    ):
        self.old_state = old_state
        self.task = task
        self.conv_handler = conv_handler
        self.conversation_data = conversation_data
        self.handler = handler
        self.update = update
        self.context = context
        self.application = application
        self.block = block

        self.task.add_done_callback(lambda done: asyncio.create_task(self.await_new_state(done)))

    async def await_new_state(self, _task: asyncio.Task) -> None:

        await self.conv_handler._update_state(  # pylint: disable=protected-access
            new_state=self.resolve(),
            conversation_data=self.conversation_data,
            update=self.update,
            context=self.context,
            block=self.block,
            application=self.application,
            handler=self.handler,
        )

    def done(self) -> bool:
        return self.task.done()

    def resolve(self) -> object:
        """Returns the new state of the :class:`ConversationHandler` if available. If there was an
        exception during the task execution, then return the old state. If both the new and old
        state are :obj:`None`, return `CH.END`. If only the new state is :obj:`None`, return the
        old state. Non-blocking entry-points with an exception will also return `CH.END`.

        Raises:
            :exc:`RuntimeError`: If the current task has not yet finished.
        """
        if not self.task.done():
            raise RuntimeError("New state is not yet available")

        exc = self.task.exception()
        if exc:
            _LOGGER.exception(
                "Task function raised exception. Falling back to old state %s",
                self.old_state,
            )

            # Special case if an error was raised in a non-blocking entry-point
            if self.old_state is None and exc:
                return ConversationHandler.END

            return self.old_state

        res = self.task.result()
        if res is None and self.old_state is None:
            res = ConversationHandler.END
        elif res is None:
            # returning None from a callback means that we want to stay in the old state
            return self.old_state

        return res


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


class ExampleConversationHandlerKey(ConversationHandlerKey):
    """Default implementation for key generation of the ConversationHandler.
    These keys are used to match Updates to a conversation.

    .. versionadded:: NEXT.VERSION
    """

    per_chat: bool = True
    per_user: bool = True
    per_message: bool = False
    user_id: Optional[int]
    chat_id: Optional[int]

    def __init__(self, per_chat: bool, per_user: bool, per_message: bool):
        """
        Initialize the ExampleConversationHandlerKey with the given parameters.

        :param per_chat: bool - Whether the function is per chat.
        :param per_user: bool - Whether the function is per user.
        :param per_message: bool - Whether the function is per message.
        """
        self.per_chat = per_chat
        self.per_user = per_user
        self.per_message = per_message

    def from_update(self, update: object) -> ConversationKey:
        """
        Builds the conversation key associated with the update.

        Parameters:
            update (:obj:`object`): The update object to build the conversation key for.

        Returns:
            ConversationKey: The conversation key associated with the update.
        """
        if not isinstance(update, Update):
            return NotImplemented

        # Builds the conversation key associated with the update.
        chat = update.effective_chat
        user = update.effective_user

        key: List[Union[int, str]] = []

        if self.per_chat:
            if chat is None:
                raise RuntimeError("Can't build key for update without effective chat!")
            key.append(chat.id)

        if self.per_user:
            if user is None:
                raise RuntimeError("Can't build key for update without effective user!")
            key.append(user.id)

        if self.per_message:
            if update.callback_query is None:
                raise RuntimeError("Can't build key for update without CallbackQuery!")
            if update.callback_query.inline_message_id:
                key.append(update.callback_query.inline_message_id)
            else:
                key.append(update.callback_query.message.message_id)  # type: ignore[union-attr]

        return tuple(key)


class ConversationHandler(BaseHandler[Update, CCT]):
    """
    A handler to hold a conversation with a single or multiple users through Telegram updates by
    managing three collections of other handlers.

    Warning:
        :class:`ConversationHandler` heavily relies on incoming updates being processed one by one.
        When using this handler, :attr:`telegram.ext.ApplicationBuilder.concurrent_updates` should
        be set to :obj:`False`.

    Note:
        :class:`ConversationHandler` will only accept updates that are (subclass-)instances of
        :class:`telegram.Update`. This is, because depending on the :attr:`per_user` and
        :attr:`per_chat`, :class:`ConversationHandler` relies on
        :attr:`telegram.Update.effective_user` and/or :attr:`telegram.Update.effective_chat` in
        order to determine which conversation an update should belong to. For
        :attr:`per_message=True <per_message>`, :class:`ConversationHandler` uses
        :attr:`update.callback_query.message.message_id <telegram.Message.message_id>` when
        :attr:`per_chat=True <per_chat>` and
        :attr:`update.callback_query.inline_message_id <.CallbackQuery.inline_message_id>` when
        :attr:`per_chat=False <per_chat>`. For a more detailed explanation, please see our `FAQ`_.

        Finally, :class:`ConversationHandler`, does *not* handle (edited) channel posts.

    .. _`FAQ`: https://github.com/python-telegram-bot/python-telegram-bot/wiki\
        /Frequently-Asked-Questions#what-do-the-per_-settings-in-conversation handler-do

    The first collection, a :obj:`list` named :attr:`entry_points`, is used to initiate the
    conversation, for example with a :class:`telegram.ext.CommandHandler` or
    :class:`telegram.ext.MessageHandler`.

    The second collection, a :obj:`list` named :attr:`pre_fallbacks`, is used if the user is
    currently in a conversation and will be checked before the states defined in :attr:`states`.
    You could use this for a ``/help`` command, which won't be checked in the :attr:`states`
    collection.

    The third collection, a :obj:`dict` named :attr:`entry_state_handlers`, contains handlers,
    which should be called on entering the corresponding states. These handlers will be called,
    as soon as the state is entered. Here you can also define an entry_state_handler for
    :attr:`TIMEOUT`, an entry_state_handler for :attr:`WAITING` and an entry_state_handler for
    :attr:`END`.

    The fourth collection, a :obj:`dict` named :attr:`states`, contains the different conversation
    steps and one or more associated handlers that should be used if the user sends a message when
    the conversation with them is currently in that state. Here you can also define a state for
    :attr:`TIMEOUT` to define the behavior when :attr:`conversation_timeout` is exceeded, and a
    state for :attr:`WAITING` to define behavior when a new update is received while the previous
    :attr:`block=False <block>` handler is not finished.

    The fifth collection, a :obj:`list` named :attr:`fallbacks`, is used if the user is currently
    in a conversation but the state has either no associated handler or the handler that is
    associated to the state is inappropriate for the update, for example if the update contains a
    command, but a regular text message is expected. You could use this for a ``/cancel`` command
    or to let the user know their message was not recognized.

    To change the state of conversation, the callback function of a handler must return the new
    state after responding to the user. If it does not return anything (returning :obj:`None` by
    default), the state will not change. If an entry point callback function returns :obj:`None`,
    the conversation ends immediately after the execution of this callback function.
    To end the conversation, the callback function must return :attr:`END` or ``-1``. To
    handle the conversation timeout, use handler :attr:`TIMEOUT` or ``-2``.
    Finally, :class:`telegram.ext.ApplicationHandlerStop` can be used in conversations as described
    in its documentation.

    Note:
        In each of the described collections of handlers, a handler may in turn be a
        :class:`ConversationHandler`. In that case, the child :class:`ConversationHandler` should
        have the attribute :attr:`map_to_parent` which allows returning to the parent conversation
        at specified states within the child conversation.

        Note that the keys in :attr:`map_to_parent` must not appear as keys in :attr:`states`
        attribute or else the latter will be ignored. You may map :attr:`END` to one of the parents
        states to continue the parent conversation after the child conversation has ended or even
        map a state to :attr:`END` to end the *parent* conversation from within the child
        conversation. For an example on nested :class:`ConversationHandler` s, see
        :any:`examples.nestedconversationbot`.

    .. versionchanged:: NEXT.VERSION
        Raise a Warning if ConversationHandler.END (-1) is used as a state.
        Timeouts for non-blocking conversations now start after the handler has finished

    Examples:
        * :any:`Conversation Bot <examples.conversationbot>`
        * :any:`Conversation Bot 2 <examples.conversationbot2>`
        * :any:`Nested Conversation Bot <examples.nestedconversationbot>`
        * :any:`Persistent Conversation Bot <examples.persistentconversationbot>`

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
        allow_reentry (:obj:`bool`, optional): If set to :obj:`True`, a user that is currently in a
            conversation can restart the conversation by triggering one of the entry points.
            Default is :obj:`False`.
        per_chat (:obj:`bool`, optional): If the conversation key should contain the Chat's ID.
            Default is :obj:`True`.
        per_user (:obj:`bool`, optional): If the conversation key should contain the User's ID.
            Default is :obj:`True`.
        per_message (:obj:`bool`, optional): If the conversation key should contain the Message's
            ID. Default is :obj:`False`.
        conversation_timeout (:obj:`float` | :obj:`datetime.timedelta`, optional): When this
            handler is inactive more than this timeout (in seconds), it will be automatically
            ended. If this value is ``0`` or :obj:`None` (default), there will be no timeout. The
            last received update and the corresponding :class:`context <.CallbackContext>` will be
            handled by *ALL* the handler's whose :meth:`check_update` method returns :obj:`True`
            that are in the state :attr:`ConversationHandler.TIMEOUT`.

            Caution:
                * This feature relies on the :attr:`telegram.ext.Application.job_queue` being set
                  and hence requires that the dependencies that :class:`telegram.ext.JobQueue`
                  relies on are installed.
                * Using :paramref:`conversation_timeout` with nested conversations is currently
                  not supported. You can still try to use it, but it will likely behave
                  differently from what you expect.

        name (:obj:`str`, optional): The name for this conversation handler. Required for
            persistence.
        persistent (:obj:`bool`, optional): If the conversation's dict for this handler should be
            saved. :paramref:`name` is required and persistence has to be set in
            :attr:`Application <.Application.persistence>`.

            .. versionchanged:: 20.0
                Was previously named as ``persistence``.
        map_to_parent (Dict[:obj:`object`, :obj:`object`], optional): A :obj:`dict` that can be
            used to instruct a child conversation handler to transition into a mapped state on
            its parent conversation handler in place of a specified nested state.
        block (:obj:`bool`, optional): Pass :obj:`False` or :obj:`True` to set a default value for
            the :attr:`BaseHandler.block` setting of all handlers (in :attr:`entry_points`,
            :attr:`pre_fallbacks`, :attr:`states` and :attr:`fallbacks`). The resolution order for
            checking if a handler should be run non-blocking is:

            1. :attr:`telegram.ext.BaseHandler.block` (if set)
            2. the value passed to this parameter (if any)
            3. :attr:`telegram.ext.Defaults.block` (if defaults are used)

            .. seealso:: :wiki:`Concurrency`

            .. versionchanged:: 20.0
                No longer overrides the handlers settings. Resolution order was changed.

    Raises:
        :exc:`ValueError`: If :paramref:`persistent` is used but :paramref:`name` was not set, or
            when :attr:`per_message`, :attr:`per_chat`, :attr:`per_user` are all :obj:`False`.

    Attributes:
        block (:obj:`bool`): Determines whether the callback will run in a blocking way. Always
            :obj:`True` since conversation handlers handle any non-blocking callbacks internally.

    """

    __slots__ = (
        "_allow_reentry",
        "_block",
        "_child_conversations",
        "_conversation_timeout",
        "_conversations",
        "_entry_points",
        "_fallbacks",
        "_map_to_parent",
        "_name",
        "_per_chat",
        "_per_message",
        "_per_user",
        "_persistent",
        "_pre_fallbacks",
        "_state_entry_handlers",
        "_states",
        "_timeout_jobs_lock",
        "key_builder",
        "timeout_jobs",
    )

    END: Final[int] = -1
    """:obj:`int`: Used as a constant to return when a conversation is ended."""
    TIMEOUT: Final[int] = -2
    """:obj:`int`: Used as a constant to handle state when a conversation is timed out
    (exceeded :attr:`conversation_timeout`).
    """
    WAITING: Final[int] = -3
    """:obj:`int`: Used as a constant to handle state when a conversation is still waiting on the
    previous :attr:`block=False <block>` handler to finish."""

    # pylint: disable=super-init-not-called
    def __init__(
        self,
        entry_points: List[BaseHandler[Update, CCT]],
        states: Dict[object, List[BaseHandler[Update, CCT]]],
        state_entry_handlers: Optional[Dict[object, List[BaseHandler[Update, CCT]]]] = None,
        pre_fallbacks: Optional[List[BaseHandler[Update, CCT]]] = None,
        fallbacks: Optional[List[BaseHandler[Update, CCT]]] = None,
        allow_reentry: bool = False,
        per_chat: bool = True,
        per_user: bool = True,
        per_message: bool = False,
        conversation_timeout: Optional[Union[float, datetime.timedelta]] = None,
        name: Optional[str] = None,
        persistent: bool = False,
        map_to_parent: Optional[Dict[object, object]] = None,
        block: DVType[bool] = DEFAULT_TRUE,
        key_builder: Optional[ConversationHandlerKey] = None,
    ):
        # these imports need to be here because of circular import error otherwise
        from telegram.ext import (  # pylint: disable=import-outside-toplevel
            PollAnswerHandler,
            PollHandler,
            PreCheckoutQueryHandler,
            ShippingQueryHandler,
        )

        if pre_fallbacks is None:
            pre_fallbacks = []

        if fallbacks is None:
            fallbacks = []

        if state_entry_handlers is None:
            state_entry_handlers = {}

        self.key_builder: ConversationHandlerKey = key_builder or ExampleConversationHandlerKey(
            per_chat=per_chat, per_user=per_user, per_message=per_message
        )

        # self.block is what the Application checks and we want it to always run CH in a blocking
        # way so that CH can take care of any non-blocking logic internally
        self.block: DVType[bool] = True
        # Store the actual setting in a protected variable instead
        self._block: DVType[bool] = block

        self._entry_points: List[BaseHandler[Update, CCT]] = entry_points
        self._pre_fallbacks: List[BaseHandler[Update, CCT]] = pre_fallbacks
        self._states: Dict[object, List[BaseHandler[Update, CCT]]] = states
        self._state_entry_handlers: Dict[object, List[BaseHandler[Update, CCT]]] = (
            state_entry_handlers
        )
        self._fallbacks: List[BaseHandler[Update, CCT]] = fallbacks

        self._allow_reentry: bool = allow_reentry
        self._per_user: bool = per_user
        self._per_chat: bool = per_chat
        self._per_message: bool = per_message
        self._conversation_timeout: Optional[Union[float, datetime.timedelta]] = (
            conversation_timeout
        )
        self._name: Optional[str] = name
        self._map_to_parent: Optional[Dict[object, object]] = map_to_parent

        # if conversation_timeout is used, this dict is used to schedule a job which runs when the
        # conv has timed out.
        self.timeout_jobs: Dict[ConversationKey, Job[Any]] = {}
        self._timeout_jobs_lock = asyncio.Lock()
        self._conversations: ConversationDict = {}
        self._child_conversations: Set[ConversationHandler] = set()

        if persistent and not self.name:
            raise ValueError("Conversations can't be persistent when handler is unnamed.")
        self._persistent: bool = persistent

        if not any((self.per_user, self.per_chat, self.per_message)):
            raise ValueError("'per_user', 'per_chat' and 'per_message' can't all be 'False'")

        if self.per_message and not self.per_chat:
            warn(
                "If 'per_message=True' is used, 'per_chat=True' should also be used, "
                "since message IDs are not globally unique.",
                stacklevel=2,
            )

        all_handlers: List[BaseHandler[Update, CCT]] = []
        all_handlers.extend(pre_fallbacks)
        all_handlers.extend(entry_points)
        all_handlers.extend(fallbacks)

        for key, state_handlers in states.items():
            all_handlers.extend(state_handlers)
            if key == self.END:
                warn(
                    f"The END state ({key!r}) is reserved and shouldn't be used as a state name"
                    f" in the `ConversationHandler`.",
                    stacklevel=2,
                )

        for state_entry_handler in state_entry_handlers.values():
            all_handlers.extend(state_entry_handler)

        self._child_conversations.update(
            handler for handler in all_handlers if isinstance(handler, ConversationHandler)
        )

        # this link will be added to all warnings tied to per_* setting
        per_faq_link = (
            " Read this FAQ entry to learn more about the per_* settings: "
            "https://github.com/python-telegram-bot/python-telegram-bot/wiki"
            "/Frequently-Asked-Questions#what-do-the-per_-settings-in-conversationhandler-do."
        )

        # this loop is going to warn the user about handlers which can work unexpectedly
        # in conversations
        for handler in all_handlers:
            if isinstance(handler, (StringCommandHandler, StringRegexHandler)):
                warn(
                    "The `ConversationHandler` only handles updates of type `telegram.Update`. "
                    f"{handler.__class__.__name__} handles updates of type `str`.",
                    stacklevel=2,
                )
            elif isinstance(handler, TypeHandler) and not issubclass(handler.type, Update):
                warn(
                    "The `ConversationHandler` only handles updates of type `telegram.Update`."
                    f" The TypeHandler is set to handle {handler.type.__name__}.",
                    stacklevel=2,
                )
            elif isinstance(handler, PollHandler):
                warn(
                    "PollHandler will never trigger in a conversation since it has no information "
                    "about the chat or the user who voted in it. Do you mean the "
                    "`PollAnswerHandler`?",
                    stacklevel=2,
                )

            elif self.per_chat and (
                isinstance(
                    handler,
                    (
                        ShippingQueryHandler,
                        InlineQueryHandler,
                        ChosenInlineResultHandler,
                        PreCheckoutQueryHandler,
                        PollAnswerHandler,
                    ),
                )
            ):
                warn(
                    f"Updates handled by {handler.__class__.__name__} only have information about "
                    "the user, so this handler won't ever be triggered if `per_chat=True`."
                    f"{per_faq_link}",
                    stacklevel=2,
                )

            elif self.per_message and not isinstance(handler, CallbackQueryHandler):
                warn(
                    "If 'per_message=True', all entry points, pre_fallbacks, state handlers"
                    " and fallbacks must be 'CallbackQueryHandler', since no other handlers "
                    f"have a message context.{per_faq_link}",
                    stacklevel=2,
                )
            elif not self.per_message and isinstance(handler, CallbackQueryHandler):
                warn(
                    "If 'per_message=False', 'CallbackQueryHandler' will not be "
                    f"tracked for every message.{per_faq_link}",
                    stacklevel=2,
                )

            if self.conversation_timeout and isinstance(handler, self.__class__):
                warn(
                    "Using `conversation_timeout` with nested conversations is currently not "
                    "supported. You can still try to use it, but it will likely behave "
                    "differently from what you expect.",
                    stacklevel=2,
                )

    def __repr__(self) -> str:
        """Give a string representation of the ConversationHandler in the form
        ``ConversationHandler[name=..., states={...}]``.

        If there are more than 3 states, only the first 3 states are listed.

        As this class doesn't implement :meth:`object.__str__`, the default implementation
        will be used, which is equivalent to :meth:`__repr__`.

        Returns:
            :obj:`str`
        """
        truncation_threshold = 3
        states = dict(list(self.states.items())[:truncation_threshold])
        states_string = str(states)
        if len(self.states) > truncation_threshold:
            states_string = states_string[:-1] + ", ...}"

        return build_repr_with_selected_attrs(
            self,
            name=self.name,
            states=states_string,
        )

    @property
    def entry_points(self) -> List[BaseHandler[Update, CCT]]:
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
    def pre_fallbacks(self) -> List[BaseHandler[Update, CCT]]:
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
    def states(self) -> Dict[object, List[BaseHandler[Update, CCT]]]:
        """Dict[:obj:`object`, List[:class:`telegram.ext.BaseHandler`]]: A :obj:`dict` that
        defines the different states of conversation a user can be in and one or more
        associated :obj:`BaseHandler` objects that should be used in that state.
        """
        return self._states

    @states.setter
    def states(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to states after initialization.")

    @property
    def state_entry_handlers(self) -> Dict[object, List[BaseHandler[Update, CCT]]]:
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
    def fallbacks(self) -> List[BaseHandler[Update, CCT]]:
        """List[:class:`telegram.ext.BaseHandler`]: A list of handlers that might be used if
        the user is in a conversation, but every handler for their current state returned
        :obj:`False` on :meth:`check_update`.
        """
        return self._fallbacks

    @fallbacks.setter
    def fallbacks(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to fallbacks after initialization.")

    @property
    def allow_reentry(self) -> bool:
        """:obj:`bool`: Determines if a user can restart a conversation with an entry point."""
        return self._allow_reentry

    @allow_reentry.setter
    def allow_reentry(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to allow_reentry after initialization."
        )

    @property
    def per_user(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the User's ID."""
        return self._per_user

    @per_user.setter
    def per_user(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_user after initialization.")

    @property
    def per_chat(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the Chat's ID."""
        return self._per_chat

    @per_chat.setter
    def per_chat(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_chat after initialization.")

    @property
    def per_message(self) -> bool:
        """:obj:`bool`: If the conversation key should contain the message's ID."""
        return self._per_message

    @per_message.setter
    def per_message(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to per_message after initialization.")

    @property
    def conversation_timeout(
        self,
    ) -> Optional[Union[float, datetime.timedelta]]:
        """:obj:`float` | :obj:`datetime.timedelta`: Optional. When this
        handler is inactive more than this timeout (in seconds), it will be automatically
        ended.
        """
        return self._conversation_timeout

    @conversation_timeout.setter
    def conversation_timeout(self, _: object) -> NoReturn:
        raise AttributeError(
            "You can not assign a new value to conversation_timeout after initialization."
        )

    @property
    def name(self) -> Optional[str]:
        """:obj:`str`: Optional. The name for this :class:`ConversationHandler`."""
        return self._name

    @name.setter
    def name(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to name after initialization.")

    @property
    def persistent(self) -> bool:
        """:obj:`bool`: Optional. If the conversations dict for this handler should be
        saved. :attr:`name` is required and persistence has to be set in
        :attr:`Application <.Application.persistence>`.
        """
        return self._persistent

    @persistent.setter
    def persistent(self, _: object) -> NoReturn:
        raise AttributeError("You can not assign a new value to persistent after initialization.")

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

    async def _initialize_persistence(
        self, application: "Application"
    ) -> Dict[str, TrackingDict[ConversationKey, ConversationData]]:
        """Initializes the persistence for this handler and its child conversations.
        While this method is marked as protected, we expect it to be called by the
        Application/parent conversations. It's just protected to hide it from users.

        Args:
            application (:class:`telegram.ext.Application`): The application.

        Returns:
            A dict {conversation.name -> TrackingDict}, which contains all dict of this
            conversation and possible child conversations.

        """
        if not (self.persistent and self.name and application.persistence):
            raise RuntimeError(
                "This handler is not persistent, has no name or the application has no "
                "persistence!"
            )

        current_conversations = self._conversations
        self._conversations = cast(
            TrackingDict[ConversationKey, ConversationData],
            TrackingDict(),
        )
        # In the conversation already processed updates
        self._conversations.update(current_conversations)
        # above might be partly overridden but that's okay since we warn about that in
        # add_handler
        stored_data = await application.persistence.get_conversations(self.name)
        # Since CH.END is stored as normal state, we need to properly parse it here in order to
        # don't add it to the _conversations dict
        # FOR BACKWARD COMPATIBILITY
        for key, conversation_data in stored_data.items():
            if not isinstance(conversation_data, ConversationData):
                warn(
                    f"Loaded persistent conversation with key `{key}` is not from type "
                    "`ConversationData`.Trying to convert it to one. You can ignore this warning"
                    "if no further errors occur",
                    stacklevel=1,
                )
                stored_data[key] = ConversationData(key=key, state=conversation_data)
        # FOR BACKWARD COMPATIBILITY
        stored_data = {key: val for key, val in stored_data.items() if val.state != self.END}
        self._conversations.update_no_track(stored_data)

        out = {self.name: self._conversations}

        await self._restore_timeout_jobs(application, stored_data)

        for handler in self._child_conversations:
            out.update(
                await handler._initialize_persistence(  # pylint: disable=protected-access
                    application=application
                )
            )

        return out

    def _schedule_job(
        self,
        new_state: object,
        application: "Application[Any, CCT, Any, Any, Any, JobQueue]",
        update: Update,
        context: CCT,
        conversation_key: ConversationKey,
        current_conversation: ConversationData,
        conversation_timeout: Optional[Union[float, datetime.timedelta]] = None,
        timeout_job_kwargs: Optional[Dict[str, Any]] = None,
    ) -> None:
        """Schedules a job which executes :meth:`_trigger_timeout` upon conversation timeout."""
        if new_state == self.END:
            return

        timeout = conversation_timeout or self.conversation_timeout
        if timeout is None:
            return

        try:
            # job_queue is checked before calling _schedule_job
            j_queue = application.job_queue
            t_job = j_queue.run_once(  # type: ignore[union-attr]
                callback=self._trigger_timeout,
                when=timeout,
                data=_ConversationTimeoutContext(conversation_key, update, application, context),
                job_kwargs=timeout_job_kwargs,
            )
            self.timeout_jobs[conversation_key] = t_job
            current_conversation.set_update(update)
            if t_job.job.pending:
                # job don't know the next run time on application startup.
                current_conversation.timeout = timeout
            else:
                current_conversation.timeout = t_job.job.next_run_time

        except Exception as exc:
            _LOGGER.exception("Failed to schedule timeout.", exc_info=exc)

    # pylint: disable=too-many-return-statements
    def check_update(self, update: object) -> Optional[_CheckUpdateType[CCT]]:
        """
        Determines whether an update should be handled by this conversation handler, and if so in
        which state the conversation currently is.

        Args:
            update (:class:`telegram.Update` | :obj:`object`): Incoming update.

        Returns:
            :obj:`bool`

        """
        if not isinstance(update, Update):
            return None
        # Ignore messages in channels
        if update.channel_post or update.edited_channel_post:
            return None
        if self.per_chat and not update.effective_chat:
            return None
        if self.per_user and not update.effective_user:
            return None
        if self.per_message and not update.callback_query:
            return None
        if update.callback_query and self.per_chat and not update.callback_query.message:
            return None

        key = self.key_builder.from_update(update)
        if key == NotImplemented:
            return None

        conversation_data = self._conversations.get(key)
        state = conversation_data.state if conversation_data is not None else None
        check: Optional[object] = None

        if isinstance(state, PendingState):
            # handle WAITING state
            handlers = self.states.get(self.WAITING, [])
            for handler_ in handlers:
                check = handler_.check_update(update)
                if check is not None and check is not False:
                    return self.WAITING, key, handler_, check
            return None

        _LOGGER.debug("Selecting conversation %s with state %s", str(key), str(state))

        handler: Optional[BaseHandler] = None

        # Search entry points for a match
        if state is None or self.allow_reentry:
            for entry_point in self.entry_points:
                check = entry_point.check_update(update)
                if check is not None and check is not False:
                    handler = entry_point
                    break

            else:
                if state is None:
                    return None

        # Get the handler list for current state, if we didn't find one yet and we're still here
        if state is not None and handler is None:

            # Find a pre_fallback handler
            for pre_fallback in self.pre_fallbacks:
                check = pre_fallback.check_update(update)
                if check is not None and check is not False:
                    handler = pre_fallback
                    break

            # find a state handler
            else:
                for candidate in self.states.get(state, []):
                    check = candidate.check_update(update)
                    if check is not None and check is not False:
                        handler = candidate
                        break

                # Find a fallback handler if all other handlers fail
                else:
                    for fallback in self.fallbacks:
                        check = fallback.check_update(update)
                        if check is not None and check is not False:
                            handler = fallback
                            break

                    else:
                        return None

        return state, key, handler, check  # type: ignore[return-value]

    async def handle_update(  # type: ignore[override]
        self,
        update: Update,
        application: "Application[Any, CCT, Any, Any, Any, Any]",
        check_result: _CheckUpdateType[CCT],
        context: CCT,
    ) -> Optional[object]:
        """Send the update to the callback for the current state and BaseHandler

        .. versionchanged:: NEXT.VERSION
            now no longer timeout jobs are created for children which are being mapped to a
            parent.
        Args:
            check_result: The result from :meth:`check_update`. For this handler it's a tuple of
                the conversation state, key, handler, and the handler's check result.
            update (:class:`telegram.Update`): Incoming telegram update.
            application (:class:`telegram.ext.Application`): Application that originated the
                update.
            context (:class:`telegram.ext.CallbackContext`): The context as provided by
                the application.

        """
        current_state, conversation_key, handler, handler_check_result = check_result
        raise_dp_handler_stop = False

        if conversation_key in self._conversations:
            current_conversation = self._conversations[conversation_key]
        else:
            # Create a new one
            current_conversation = ConversationData(
                key=conversation_key,
                state=None,
                update=None,
                timeout=None,
            )
            self._conversations[conversation_key] = current_conversation

        async with self._timeout_jobs_lock:
            # Remove the old timeout job (if present)
            timeout_job = self.timeout_jobs.pop(conversation_key, None)

            if timeout_job is not None:
                timeout_job.schedule_removal()
            current_conversation.timeout = None
            current_conversation.set_update(None)

        # Resolution order of "block":
        # 1. Setting of the selected handler
        # 2. Setting of the ConversationHandler
        # 3. Default values of the bot
        if handler.block is not DEFAULT_TRUE:
            block = handler.block
        elif self._block is not DEFAULT_TRUE:
            block = self._block
        elif isinstance(application.bot, ExtBot) and application.bot.defaults is not None:
            block = application.bot.defaults.block
        else:
            block = DefaultValue.get_value(handler.block)

        try:  # Now create task or await the callback
            if block:
                new_state: object = await handler.handle_update(
                    update, application, handler_check_result, context
                )
            else:
                new_state = application.create_task(
                    coroutine=handler.handle_update(
                        update, application, handler_check_result, context
                    ),
                    update=update,
                    name=f"ConversationHandler:{update.update_id}:handle_update:non_blocking_cb",
                )
        except ApplicationHandlerStop as exception:
            new_state = exception.state
            raise_dp_handler_stop = True

        if isinstance(self.map_to_parent, dict) and new_state in self.map_to_parent:
            await self._update_state(
                new_state=self.END,
                conversation_data=current_conversation,
                update=update,
                context=context,
                block=block,
                application=application,
            )

            if raise_dp_handler_stop:
                raise ApplicationHandlerStop(self.map_to_parent.get(new_state))
            return self.map_to_parent.get(new_state)

        if current_state != self.WAITING:
            await self._update_state(
                new_state=new_state,
                conversation_data=current_conversation,
                update=update,
                context=context,
                block=block,
                application=application,
                handler=handler,
            )

        if raise_dp_handler_stop:
            # Don't pass the new state here. If we're in a nested conversation, the parent is
            # expecting None as return value.
            raise ApplicationHandlerStop
        # Signals a possible parent conversation to stay in the current state
        return None

    async def _update_state(
        self,
        new_state: object,
        conversation_data: ConversationData,
        update: Update,
        context: CCT,
        block: DVType[bool],
        application: "Application",
        handler: Optional[BaseHandler] = None,
    ) -> None:
        if isinstance(new_state, asyncio.Task):
            conversation_data.state = PendingState(
                conversation_data=conversation_data,
                old_state=conversation_data.state,
                task=new_state,
                conv_handler=self,
                handler=handler,
                update=update,
                context=context,
                application=application,
                block=block,
            )
            self._conversations[conversation_data.key] = conversation_data
            return
            # we will come back once the state is resolved

        result = await self._execute_state_entry_handlers(
            new_state=new_state,
            update=update,
            context=context,
            application=application,
        )
        if result is not None:
            if new_state != self.END:
                new_state = result
            else:
                warn(
                    "State END (-1) can not be changed by state_entry_handlers.",
                    stacklevel=2,
                )

        if new_state == self.END:
            if conversation_data.key in self._conversations:
                # If there is no key in conversations, nothing is done.
                del self._conversations[conversation_data.key]
                return

        elif new_state is not None:
            if new_state not in self.states:
                warn(
                    f"{repr(handler.callback.__name__) if handler is not None else 'BaseHandler'} "
                    f"returned state {new_state} which is unknown to the "
                    f"ConversationHandler{' ' + self.name if self.name is not None else ''}.",
                    stacklevel=2,
                )
            conversation_data.state = new_state

        if self.conversation_timeout:
            async with self._timeout_jobs_lock:
                if application.job_queue is None:
                    warn(
                        "Ignoring `conversation_timeout` because the Application has no JobQueue.",
                        stacklevel=1,
                    )
                elif not application.job_queue.scheduler.running:
                    warn(
                        "Ignoring `conversation_timeout` because the Applications JobQueue is "
                        "not running.",
                        stacklevel=1,
                    )
                else:
                    self._schedule_job(
                        new_state=new_state,
                        application=application,
                        update=update,
                        context=context,
                        conversation_key=conversation_data.key,
                        current_conversation=conversation_data,
                    )

        self._conversations[conversation_data.key] = conversation_data

    async def _trigger_timeout(self, context: CCT) -> None:
        """This is run whenever a conversation has timed out. Also makes sure that all handlers
        which are in the :attr:`TIMEOUT` state and whose :meth:`BaseHandler.check_update` returns
        :obj:`True` is handled.
        """
        job = cast("Job", context.job)
        ctxt = cast(_ConversationTimeoutContext, job.data)

        _LOGGER.debug(
            "Conversation timeout was triggered for conversation %s!", ctxt.conversation_key
        )

        callback_context = ctxt.callback_context

        async with self._timeout_jobs_lock:
            found_job = self.timeout_jobs.get(ctxt.conversation_key)
            if found_job is not job:
                # The timeout has been cancelled in handle_update
                return
            del self.timeout_jobs[ctxt.conversation_key]

        # Now run all handlers which are in TIMEOUT state
        handlers = self.state_entry_handlers.get(self.TIMEOUT, [])
        handlers.extend(self.states.get(self.TIMEOUT, []))
        for handler in handlers:
            check = handler.check_update(ctxt.update)
            if check is not None and check is not False:
                try:
                    await handler.handle_update(
                        ctxt.update, ctxt.application, check, callback_context
                    )
                except ApplicationHandlerStop:
                    warn(
                        "ApplicationHandlerStop in TIMEOUT state of "
                        "ConversationHandler has no effect. Ignoring.",
                        stacklevel=2,
                    )

        conversation_data = self._conversations[ctxt.conversation_key]

        await self._update_state(
            new_state=self.END,
            conversation_data=conversation_data,
            update=ctxt.update,
            context=callback_context,
            block=True,
            application=ctxt.application,
        )

    async def _execute_state_entry_handlers(
        self,
        new_state: object,
        update: Update,
        context: CCT,
        application: "Application",
    ) -> Optional[object]:
        state_entry_handlers = self.state_entry_handlers.get(new_state, [])
        for state_entry_handler in state_entry_handlers:
            check = state_entry_handler.check_update(update)
            if check is not None and check is not False:
                try:
                    return await state_entry_handler.handle_update(
                        update, application, check, context
                    )
                except ApplicationHandlerStop:
                    warn(
                        "ApplicationHandlerStop in State_entry_handlers has no effect. Ignoring.",
                        stacklevel=2,
                    )
                break
        return None

    async def _restore_timeout_jobs(
        self, application: "Application", stored_conversations: ConversationDict
    ) -> None:
        if not (self.persistent and self.name and application.persistence):
            raise RuntimeError(
                "This handler is not persistent, has no name or the application has no "
                "persistence!"
            )
        for key, conversation_data in stored_conversations.items():
            if conversation_data.state is None:
                continue
            update = conversation_data.update_as_object(application.bot)
            if update is None:
                continue
            context = application.context_types.context.from_update(
                update=update, application=application
            )
            self._schedule_job(
                new_state=conversation_data.state,
                application=application,
                update=update,
                context=context,
                conversation_key=key,
                current_conversation=conversation_data,
                conversation_timeout=conversation_data.timeout,
                timeout_job_kwargs={"misfire_grace_time": None},
            )

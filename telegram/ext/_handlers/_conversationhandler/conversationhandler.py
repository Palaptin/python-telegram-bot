#!/usr/bin/env python
#
# A library that provides a Python interface to the Telegram Bot API
# Copyright (C) 2015-2025
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
import datetime as dtm
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Final, Generic, NoReturn, Optional, Union, cast

from telegram import Update
from telegram._utils.defaultvalue import DEFAULT_TRUE, DefaultValue
from telegram._utils.logging import get_logger
from telegram._utils.repr import build_repr_with_selected_attrs
from telegram._utils.types import DVType
from telegram._utils.warnings import warn
from telegram.ext._application import ApplicationHandlerStop
from telegram.ext._extbot import ExtBot
from telegram.ext._handlers._conversationhandler.conversationdata import ConversationData
from telegram.ext._handlers._conversationhandler.conversationhandlerkey import (
    ConversationHandlerKey,
)
from telegram.ext._handlers._conversationhandler.pendingstate import PendingState
from telegram.ext._handlers.basehandler import BaseHandler
from telegram.ext._utils.trackingdict import TrackingDict
from telegram.ext._utils.types import CCT, COD, ConversationDict, ConversationKey

if TYPE_CHECKING:
    from telegram.ext import Application, ConversationStates, Job, JobQueue
_CheckUpdateType = tuple[object, ConversationKey, BaseHandler[object, CCT, object], object]

_LOGGER = get_logger(__name__, class_name="ConversationHandler")


@dataclass
class _ConversationTimeoutContext(Generic[CCT]):
    """Used as a datastore for conversation timeouts. Passed in the
    :paramref:`JobQueue.run_once.data` parameter. See :meth:`_trigger_timeout`.
    """

    __slots__ = ("application", "callback_context", "conversation_key", "update")

    conversation_key: ConversationKey
    update: object
    application: "Application[Any, CCT, Any, Any, Any, JobQueue]"
    callback_context: CCT


class ConversationHandler(BaseHandler[object, CCT, object]):
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
    :attr:`END`. The Update received, as well as the context will be the same, as the one from the
    handler which transferred to this state

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
        conversation_timeout (:obj:`float` | :obj:`datetime.timedelta`, optional): When this
            handler is inactive more than this timeout (in seconds), it will be automatically
            ended. If this value is ``0`` or :obj:`None` (default), there will be no timeout. The
            last received update and the corresponding :class:`context <.CallbackContext>` will be
            handled by the first handler  in the state :attr:`ConversationHandler.TIMEOUT` whose
            :meth:`check_update` method returns :obj:`True`.


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
        conversation_context_data_type (:obj:`type`, optional): Determines the type of
            :attr:`ConversationData.conversation_context_data
            <CallbackContext.ConversationData.conversation_context_data>` of all (error-) handler
            callbacks and job callbacks. Defaults to :obj:`dict`. Must support instantiating
            without arguments.
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
        "_block",
        "_conversation_timeout",
        "_conversations",
        "_name",
        "_persistent",
        "_timeout_jobs_lock",
        "available_states",
        "conversation_context_data_type",
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
        self: "ConversationHandler[CCT]",
        conversation_states: "ConversationStates[CCT]",
        key_builder: Optional[ConversationHandlerKey] = None,
        conversation_timeout: Optional[Union[float, dtm.timedelta]] = None,
        name: Optional[str] = None,
        persistent: bool = False,
        block: DVType[bool] = DEFAULT_TRUE,
        conversation_context_data_type: Optional[COD] = None,
    ):
        # these imports need to be here because of circular import error otherwise
        # pylint: disable=import-outside-toplevel
        from telegram.ext._handlers._conversationhandler.defaultconversationhandlerkey import (
            DefaultConversationHandlerKey,
        )

        self.available_states = conversation_states

        self.key_builder: ConversationHandlerKey = key_builder or DefaultConversationHandlerKey()
        self.conversation_context_data_type = conversation_context_data_type

        # self.block is what the Application checks and we want it to always run CH in a blocking
        # way so that CH can take care of any non-blocking logic internally
        self.block: DVType[bool] = True
        # Store the actual setting in a protected variable instead
        self._block: DVType[bool] = block

        self._conversation_timeout: Optional[Union[float, dtm.timedelta]] = conversation_timeout
        self._name: Optional[str] = name

        # if conversation_timeout is used, this dict is used to schedule a job which runs when the
        # conv has timed out.
        self.timeout_jobs: dict[ConversationKey, Job[Any]] = {}
        self._timeout_jobs_lock = asyncio.Lock()
        self._conversations: ConversationDict = {}

        if persistent and not self.name:
            raise ValueError("Conversations can't be persistent when handler is unnamed.")
        self._persistent: bool = persistent

        # this loop is going to warn the user about handlers which can work unexpectedly
        # in conversations
        for handler in self.available_states.get_all_handlers():

            if self.conversation_timeout and isinstance(handler, self.__class__):
                warn(
                    "Using `conversation_timeout` with nested conversations is currently not "
                    "supported. You can still try to use it, but it will likely behave "
                    "differently from what you expect.",
                    stacklevel=2,
                )
            self.key_builder.warn_if_handler_is_invalid(handler)

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
        states = dict(list(self.available_states.states.items())[:truncation_threshold])
        states_string = str(states)
        if len(self.available_states.states) > truncation_threshold:
            states_string = states_string[:-1] + ", ...}"

        return build_repr_with_selected_attrs(
            self,
            name=self.name,
            states=states_string,
        )

    @property
    def conversation_timeout(
        self,
    ) -> Optional[Union[float, dtm.timedelta]]:
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

    async def _initialize_persistence(
        self, application: "Application"
    ) -> dict[str, TrackingDict[ConversationKey, ConversationData]]:
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
        # FOR BACKWARD COMPATIBILITY
        for key, conversation_data in stored_data.items():
            if not isinstance(conversation_data, ConversationData):
                warn(
                    f"Loaded persistent conversation with key `{key}` is not from type "
                    "`ConversationData`.Trying to convert it to one. You can ignore this warning"
                    "if no further errors occur",
                    stacklevel=1,
                )
                stored_data[key] = ConversationData(
                    key=key,
                    state=conversation_data,
                    conversation_context_data=self.conversation_context_data_type,
                )
        # FOR BACKWARD COMPATIBILITY
        # Since CH.END is stored as normal state, we need to properly parse it here in order to
        # don't add it to the _conversations dict
        stored_data = {key: val for key, val in stored_data.items() if val.state != self.END}
        self._conversations.update_no_track(stored_data)

        out = {self.name: self._conversations}

        await self._restore_timeout_jobs(application, stored_data)

        for handler in self.available_states.get_child_conversations():
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
        update: object,
        context: CCT,
        conversation_key: ConversationKey,
        current_conversation: ConversationData,
        timeout_job_kwargs: Optional[dict[str, Any]] = None,
    ) -> None:
        """Schedules a job which executes :meth:`_trigger_timeout` upon conversation timeout."""
        if new_state == self.END:
            return

        timeout = current_conversation.timeout or self.conversation_timeout
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
            current_conversation.update = update
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

        key = self.key_builder.from_update(update)
        if key == NotImplemented:
            return None

        conversation_data = self._conversations.get(key)
        state = conversation_data.state if conversation_data is not None else None
        check: Optional[object] = None

        if isinstance(state, PendingState):
            # handle WAITING state
            handlers = self.available_states.states.get(self.WAITING, [])
            for handler_ in handlers:
                check = handler_.check_update(update)
                if check is not None and check is not False:
                    return self.WAITING, key, handler_, check
            return None

        _LOGGER.debug("Selecting conversation %s with state %s", str(key), str(state))

        handler: Optional[BaseHandler] = None

        # Search entry points for a match
        if state is None or self.available_states.allow_reentry:
            for entry_point in self.available_states.entry_points:
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
            for pre_fallback in self.available_states.pre_fallbacks:
                check = pre_fallback.check_update(update)
                if check is not None and check is not False:
                    handler = pre_fallback
                    break

            # find a state handler
            else:
                for candidate in self.available_states.states.get(state, []):
                    check = candidate.check_update(update)
                    if check is not None and check is not False:
                        handler = candidate
                        break

                # Find a fallback handler if all other handlers fail
                else:
                    for fallback in self.available_states.fallbacks:
                        check = fallback.check_update(update)
                        if check is not None and check is not False:
                            handler = fallback
                            break

                    else:
                        return None

        return state, key, handler, check  # type: ignore[return-value]

    async def handle_update(  # type: ignore[override]
        self,
        update: object,
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
                conversation_context_data=self.conversation_context_data_type,
                update=None,
                timeout=None,
            )
            self._conversations[conversation_key] = current_conversation

        context._conversation_data = current_conversation  # pylint: disable=protected-access

        async with self._timeout_jobs_lock:
            # Remove the old timeout job (if present)
            timeout_job = self.timeout_jobs.pop(conversation_key, None)

            if timeout_job is not None:
                timeout_job.schedule_removal()
            current_conversation.timeout = None
            current_conversation.update = None

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
                update_id = update.update_id if isinstance(update, Update) else None
                new_state = application.create_task(
                    coroutine=handler.handle_update(
                        update, application, handler_check_result, context
                    ),
                    update=update,
                    name=f"ConversationHandler:{update_id}:handle_update:non_blocking_cb",
                )
        except ApplicationHandlerStop as exception:
            new_state = exception.state
            raise_dp_handler_stop = True
        result = None
        if current_state != self.WAITING:
            result = await self._update_state(
                new_state=new_state,
                conversation_data=current_conversation,
                original_conv_key=conversation_key,
                update=update,
                context=context,
                block=block,
                application=application,
                handler=handler,
            )

        if raise_dp_handler_stop:
            if (
                isinstance(self.available_states.map_to_parent, dict)
                and new_state in self.available_states.map_to_parent
            ):
                raise ApplicationHandlerStop(self.available_states.map_to_parent.get(new_state))
            # Don't pass the new state here. If we're in a nested conversation, the parent is
            # expecting None as return value.
            raise ApplicationHandlerStop
        # Signals a possible parent conversation to stay in the current state
        return result

    async def _update_state(
        self,
        new_state: object,
        conversation_data: ConversationData,
        update: object,
        context: CCT,
        block: DVType[bool],
        application: "Application",
        handler: Optional[BaseHandler] = None,
        original_conv_key: Optional[ConversationKey] = None,
    ) -> Optional[object]:

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
            return None
            # we will come back once the state is resolved

        result = await self._execute_state_entry_handlers(
            new_state=new_state,
            update=update,
            context=context,
            application=application,
        )
        if result is not None:
            new_state = result

        if (
            isinstance(self.available_states.map_to_parent, dict)
            and new_state in self.available_states.map_to_parent
        ):
            if conversation_data.key in self._conversations:
                # If there is no key in conversations, nothing is done.
                del self._conversations[conversation_data.key]
            return self.available_states.map_to_parent.get(new_state)

        if new_state == self.END and conversation_data.key in self._conversations:
            # If there is no key in conversations, nothing is done.
            del self._conversations[conversation_data.key]
            return None

        if new_state is not None:
            if new_state not in self.available_states.states:
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

        if original_conv_key and original_conv_key != conversation_data.key:
            del self._conversations[original_conv_key]

        self._conversations[conversation_data.key] = conversation_data
        return None

    async def _trigger_timeout(self, context: CCT) -> None:
        """This is run whenever a conversation has timed out. Also makes sure that the first
        matching handler which is in the :attr:`TIMEOUT` state and whose
         :meth:`BaseHandler.check_update` returns :obj:`True` is handled.
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

        # Now run the first matching handler which is in TIMEOUT state
        new_state: Optional[object] = None
        handlers = self.available_states.states.get(self.TIMEOUT, [])
        for handler in handlers:
            check = handler.check_update(ctxt.update)
            if check is not None and check is not False:
                try:
                    new_state = await handler.handle_update(
                        ctxt.update, ctxt.application, check, callback_context
                    )
                except ApplicationHandlerStop:
                    warn(
                        "ApplicationHandlerStop in TIMEOUT state of "
                        "ConversationHandler has no effect. Ignoring.",
                        stacklevel=2,
                    )
                break

        conversation_data = self._conversations[ctxt.conversation_key]

        await self._update_state(
            new_state=new_state if new_state is not None else self.END,
            conversation_data=conversation_data,
            update=ctxt.update,
            context=callback_context,
            block=True,
            application=ctxt.application,
        )

    async def _execute_state_entry_handlers(
        self,
        new_state: object,
        update: object,
        context: CCT,
        application: "Application",
    ) -> Optional[object]:
        state_entry_handlers = self.available_states.state_entry_handlers.get(new_state, [])
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
            update = conversation_data.update
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
                timeout_job_kwargs={"misfire_grace_time": None},
            )

    async def get_conversation(self, key: ConversationKey) -> Optional[ConversationData]:
        return self._conversations.get(key)

    async def update_conversation(
        self, key: ConversationKey, conv_data: ConversationData, application: "Application"
    ) -> None:

        context = application.context_types.context.from_update(
            update=conv_data.update, application=application
        )

        if key in self._conversations:
            current_conversation = self._conversations[key]
        else:
            # Create a new one
            current_conversation = ConversationData(
                key=key,
                state=None,
                conversation_context_data=self.conversation_context_data_type,
                update=None,
                timeout=None,
            )
            self._conversations[key] = current_conversation

        context._conversation_data = current_conversation  # pylint: disable=protected-access

        await self._update_state(
            new_state=conv_data.state,
            conversation_data=conv_data,
            update=conv_data.update,
            context=context,
            block=self.block,
            application=application,
            original_conv_key=key,
        )

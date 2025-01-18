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
from typing import Any
from unittest.mock import Mock
from warnings import filterwarnings

import pytest
from _pytest.recwarn import WarningsRecorder

from telegram import CallbackQuery, Chat, Message, Update, User
from telegram.ext import (
    CallbackQueryHandler,
    ChosenInlineResultHandler,
    CommandHandler,
    DefaultConversationHandlerKey,
    InlineQueryHandler,
    PollAnswerHandler,
    PollHandler,
    PreCheckoutQueryHandler,
    ShippingQueryHandler,
    StringCommandHandler,
    StringRegexHandler,
    TypeHandler,
)
from telegram.warnings import PTBUserWarning
from tests.auxil.slots import mro_slots


class TestDefaultConversationHandlerKeyWithoutRequest:

    async def callback(self, _: Any, __: Any) -> None:
        return

    def test_slot_behaviour(self) -> None:
        default_key = DefaultConversationHandlerKey()
        for attr in default_key.__slots__:
            assert getattr(default_key, attr, "err") != "err", f"got extra slot '{attr}'"
        assert len(mro_slots(default_key)) == len(set(mro_slots(default_key))), "duplicate slot"

    def test_init(self) -> None:

        dk = DefaultConversationHandlerKey(
            per_chat="per_chat", per_user="per_user", per_message="per_message"  # type: ignore
        )
        assert dk.per_chat == "per_chat"
        assert dk.per_user == "per_user"
        assert dk.per_message == "per_message"

    def test_init_all_false(self) -> None:
        with pytest.raises(ValueError, match="can't all be 'False'"):
            DefaultConversationHandlerKey(per_chat=False, per_user=False, per_message=False)

    def test_init_per_message_without_per_chat(self, recwarn: WarningsRecorder) -> None:
        # If per_message is True, per_chat should also be True, since msg ids are not unique
        recwarn.clear()
        DefaultConversationHandlerKey(per_message=True, per_chat=False)
        assert str(recwarn[0].message) == (
            "If 'per_message=True' is used, 'per_chat=True' should also be used, "
            "since message IDs are not globally unique."
        )

    @pytest.mark.parametrize("attr", ["per_chat", "per_user", "per_message"], indirect=False)
    def test_immutable(self, attr: str) -> None:
        ds = DefaultConversationHandlerKey()
        print(attr)
        with pytest.raises(AttributeError, match=f"You can not assign a new value to {attr}"):
            setattr(ds, attr, True)

    @pytest.mark.parametrize("increase_stack", [True, False], indirect=False)
    def test_warn_if_handler_is_invalid(
        self, recwarn: WarningsRecorder, increase_stack: bool, recursion: bool = False
    ) -> None:
        """this function tests all handler + per_* setting combinations."""
        if increase_stack:
            # increase stack to get wrong stacklevel on the warnings
            self.test_warn_if_handler_is_invalid(recwarn, False, True)
            return
        per_faq_link = (
            " Read this FAQ entry to learn more about the per_* settings: "
            "https://github.com/python-telegram-bot/python-telegram-bot/wiki"
            "/Frequently-Asked-Questions#what-do-the-per_-settings-in-conversationhandler-do."
        )

        # the warning message action needs to be set to always,
        # otherwise only the first occurrence will be issued
        filterwarnings(action="always", category=PTBUserWarning)

        recwarn.clear()

        # these handlers should all raise a warning when per_chat is True
        dk = DefaultConversationHandlerKey(per_chat=True)
        dk.warn_if_handler_is_invalid(ShippingQueryHandler(callback=self.callback))
        assert str(recwarn[0].message) == (
            "Updates handled by ShippingQueryHandler only have information about the user,"
            " so this handler won't ever be triggered if `per_chat=True`." + per_faq_link
        )
        dk.warn_if_handler_is_invalid(InlineQueryHandler(callback=self.callback))
        assert str(recwarn[1].message) == (
            "Updates handled by InlineQueryHandler only have information about the user,"
            " so this handler won't ever be triggered if `per_chat=True`." + per_faq_link
        )
        dk.warn_if_handler_is_invalid(PreCheckoutQueryHandler(callback=self.callback))
        assert str(recwarn[2].message) == (
            "Updates handled by PreCheckoutQueryHandler only have information about the user,"
            " so this handler won't ever be triggered if `per_chat=True`." + per_faq_link
        )

        dk.warn_if_handler_is_invalid(PollAnswerHandler(callback=self.callback))
        assert str(recwarn[3].message) == (
            "Updates handled by PollAnswerHandler only have information about the user,"
            " so this handler won't ever be triggered if `per_chat=True`." + per_faq_link
        )
        dk.warn_if_handler_is_invalid(ChosenInlineResultHandler(callback=self.callback))
        assert str(recwarn[4].message) == (
            "Updates handled by ChosenInlineResultHandler only have information about the user,"
            " so this handler won't ever be triggered if `per_chat=True`." + per_faq_link
        )

        # the CallbackQueryHandler should *not* raise when per_message is True,
        # but any other one should
        dk = DefaultConversationHandlerKey(per_message=True)
        dk.warn_if_handler_is_invalid(CallbackQueryHandler(callback=self.callback))
        with pytest.raises(IndexError, match="list index out of range"):
            # there should be no error
            _ = recwarn[5]
        dk.warn_if_handler_is_invalid(CommandHandler(command="t", callback=self.callback))
        assert str(recwarn[5].message) == (
            "If 'per_message=True', all entry points, pre_fallbacks, state handlers and fallbacks"
            " must be 'CallbackQueryHandler', since no other handlers have a message context."
            + per_faq_link
        )

        # the CallbackQueryHandler should raise when per_message is False, but any other should not
        dk = DefaultConversationHandlerKey(per_message=False)
        dk.warn_if_handler_is_invalid(CommandHandler(command="t", callback=self.callback))
        with pytest.raises(IndexError, match="list index out of range"):
            # there should be no error
            _ = recwarn[6]
        dk.warn_if_handler_is_invalid(CallbackQueryHandler(callback=self.callback))
        assert str(recwarn[6].message) == (
            "If 'per_message=False', 'CallbackQueryHandler' will not be tracked for every message."
            + per_faq_link
        )

        # Following are the string_regex, Pollhandler and TypeHandler which should all generate a
        # warning. TypeHandler should not when the class is Update
        dk.warn_if_handler_is_invalid(StringCommandHandler("code", self.callback))
        assert (
            str(recwarn[7].message)
            == "The `ConversationHandler` only handles updates of type `telegram.Update`. "
            "StringCommandHandler handles updates of type `str`."
        )
        dk.warn_if_handler_is_invalid(StringRegexHandler("code", self.callback))
        assert (
            str(recwarn[8].message)
            == "The `ConversationHandler` only handles updates of type `telegram.Update`. "
            "StringRegexHandler handles updates of type `str`."
        )
        dk.warn_if_handler_is_invalid(PollHandler(self.callback))
        assert (
            str(recwarn[9].message)
            == "PollHandler will never trigger in a conversation since it has no information "
            "about the chat or the user who voted in it. Do you mean the "
            "`PollAnswerHandler`?"
        )

        # this class doesn't do anything, it's just not the Update class
        class NotUpdate:
            pass

        dk.warn_if_handler_is_invalid(TypeHandler(NotUpdate, self.callback))
        assert (
            str(recwarn[10].message)
            == "The `ConversationHandler` only handles updates of type `telegram.Update`. "
            "The TypeHandler is set to handle NotUpdate."
        )
        dk.warn_if_handler_is_invalid(TypeHandler(Update, self.callback))
        with pytest.raises(IndexError, match="list index out of range"):
            # there should be no error
            _ = recwarn[11]

        # Check there ar not more errors
        assert len(recwarn) == 11

        # this for loop checks if the correct stack level is used when generating the warning
        for warning in recwarn:
            assert warning.category is PTBUserWarning
            # stack for warnings is 3 in this handler.
            if recursion:
                assert warning.filename == __file__, "incorrect stack level!"
            else:
                # If there is no Recursion, the warning will point to the python internal files
                assert warning.filename != __file__, "incorrect stack level!"

    @pytest.mark.parametrize("per_chat", [True, False])
    @pytest.mark.parametrize("per_user", [True, False])
    @pytest.mark.parametrize("per_message", [True, False])
    @pytest.mark.filterwarnings("ignore: If 'per_message=True' is used, 'per_chat=True'")
    def test_from_update(self, per_chat: bool, per_user: bool, per_message: bool) -> None:

        def evaluation_helper(
            chat: bool = False, user: bool = False, message: bool = False
        ) -> tuple:
            # helper to evaluate if the update should have been handled or not
            result: list[int] = []
            if per_chat:
                if not chat:
                    return NotImplemented
                result.append(1)
            if per_user:
                if not user:
                    return NotImplemented
                result.append(2)
            if per_message:
                if not message:
                    return NotImplemented
                result.append(3)
            return tuple(result)

        if not any((per_chat, per_user, per_message)):
            with pytest.raises(
                ValueError, match="'per_user', 'per_chat' and 'per_message' can't all be 'False'"
            ):
                DefaultConversationHandlerKey(
                    per_chat=per_chat, per_user=per_user, per_message=per_message
                )
            return

        dk = DefaultConversationHandlerKey(
            per_chat=per_chat, per_user=per_user, per_message=per_message
        )
        assert dk.from_update(object()) == NotImplemented  # Not an Instance of Update

        empty_update = Update(update_id=1)
        assert dk.from_update(empty_update) == NotImplemented  # Update has no Content to match

        # Test per_chat
        update = Mock(spec=Update)
        update.effective_chat = Mock(spec=Chat)
        update.effective_chat.id = 1
        update.effective_user = False
        update.callback_query = False
        assert dk.from_update(update) == evaluation_helper(chat=True)

        # Test per_user
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = False
        assert dk.from_update(update) == evaluation_helper(user=True)

        # Test per_message(inline)
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = False
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = 3
        assert dk.from_update(update) == evaluation_helper(message=True)

        # Test per_message(message)
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = False
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = False
        update.callback_query.message = Mock(spec=Message)
        update.callback_query.message.message_id = 3
        assert dk.from_update(update) == evaluation_helper(message=True)

        # Test per_message(None)
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = False
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = False
        update.callback_query.message = False
        assert dk.from_update(update) == NotImplemented

        # Test per_chat + per_user
        update = Mock(spec=Update)
        update.effective_chat = Mock(speck=Chat)
        update.effective_chat.id = 1
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = False
        assert dk.from_update(update) == evaluation_helper(chat=True, user=True)

        # Test per_chat + per_message
        update = Mock(spec=Update)
        update.effective_chat = Mock(spec=Chat)
        update.effective_chat.id = 1
        update.effective_user = False
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = 3
        assert dk.from_update(update) == evaluation_helper(chat=True, message=True)

        # Test per_user + per_message(inline)
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = 3
        assert dk.from_update(update) == evaluation_helper(user=True, message=True)

        # Test per_user + per_message(message)
        update = Mock(spec=Update)
        update.effective_chat = False
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = False
        update.callback_query.message = Mock(spec=Message)
        update.callback_query.message.message_id = 3
        assert dk.from_update(update) == evaluation_helper(user=True, message=True)

        # Test per_chat + per_user + per_message(inline)
        update = Mock(spec=Update)
        update.effective_chat = Mock(spec=Chat)
        update.effective_chat.id = 1
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = 3
        assert dk.from_update(update) == evaluation_helper(chat=True, user=True, message=True)

        # Test per_chat + per_user + per_message(message)
        update = Mock(spec=Update)
        update.effective_chat = Mock(spec=Chat)
        update.effective_chat.id = 1
        update.effective_user = Mock(spec=User)
        update.effective_user.id = 2
        update.callback_query = Mock(spec=CallbackQuery)
        update.callback_query.inline_message_id = False
        update.callback_query.message = Mock(spec=Message)
        update.callback_query.message.message_id = 3
        assert dk.from_update(update) == evaluation_helper(chat=True, user=True, message=True)

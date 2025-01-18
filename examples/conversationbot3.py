#!/usr/bin/env python
# This program is dedicated to the public domain under the CC0 license.

"""
First, a few callback functions are defined. Then, those functions are passed to
the Application and registered at their respective places.
Then, the bot is started and runs until we press Ctrl-C on the command line.

Usage:
Example of a bot-user conversation using ConversationHandler.
Send /start to initiate the conversation.
Press Ctrl-C on the command line or send a signal to the process to stop the
bot.
"""

import logging
import uuid
from typing import ClassVar, Optional

from telegram import ReplyKeyboardMarkup, ReplyKeyboardRemove, Update
from telegram.ext import (
    Application,
    CommandHandler,
    ContextTypes,
    ConversationHandler,
    ConversationStates,
    DefaultConversationHandlerKey,
    MessageHandler,
    TypeHandler,
    filters,
)
from telegram.ext._utils.types import ConversationKey

# Enable logging
logging.basicConfig(
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s", level=logging.INFO
)
# set higher logging level for httpx to avoid all GET and POST requests being logged
logging.getLogger("httpx").setLevel(logging.WARNING)

logger = logging.getLogger(__name__)

CHOOSE, START_CHAT, CHAT = range(3)

reply_keyboard = [
    ["Host a chat"],
    ["Exit"],
]
markup = ReplyKeyboardMarkup(reply_keyboard, one_time_keyboard=True)


class ConvHandlerDummy:
    conv_handler: ClassVar[ConversationHandler]


class InviteLinkStore:
    """Helper Class, which temporarily stores the invite links for later access"""

    store: ClassVar[dict[str, tuple]] = {}

    @classmethod
    def add_invite_link(cls, invite_link: str, key: ConversationKey) -> None:
        """Add the invitelink together with the ConversationKey, to be able to modify this
        conversation later on"""
        cls.store[invite_link] = key

    @classmethod
    def get_conversation_key(cls, invite_link: str) -> ConversationKey:
        """Get the corresponding ConversationKey"""
        return cls.store.get(invite_link, None)


class KeyBuilder(DefaultConversationHandlerKey):
    player_store: ClassVar[dict[int, str]] = {}

    def from_update(self, update: object) -> ConversationKey:
        if isinstance(update, Update):
            if update.effective_message.text.startswith("/start_chat_with "):
                invite_link = update.effective_message.text.removeprefix("/start_chat_with ")
                return InviteLinkStore.get_conversation_key(invite_link)
            if self.player_store.get(update.effective_user.id):
                key = self.player_store[update.effective_user.id]
                for player, player_key in self.player_store.items():
                    if player_key == key and player != update.effective_user.id:
                        return key  # type: ignore

        return super().from_update(update)


key_builder = KeyBuilder()


def get_invitelink(context: ContextTypes.DEFAULT_TYPE) -> str:
    """Helper function to create an invitelink"""
    invite_link = str(uuid.uuid4())
    InviteLinkStore.add_invite_link(invite_link, context.conversation_data.key)
    return invite_link


async def start(update: Update, _context: ContextTypes.DEFAULT_TYPE) -> int:
    """Start the conversation and ask user for input."""
    await update.message.reply_text(
        "Hi! You can either choose to to host a chat, or join one by pasting the invite link.",
        reply_markup=markup,
    )
    return CHOOSE


async def host_chat(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    """The user decided to host a conversation, so give him the invite link"""
    invite_link = get_invitelink(context)
    await update.effective_message.reply_text(
        "Send the following invitelink to the user U wish to play with: <code> /start_chat_with "
        + invite_link
        + "</code>",
        parse_mode="HTML",
    )
    key_builder.player_store[update.effective_user.id] = invite_link


async def join_chat(update: Update, context: ContextTypes.DEFAULT_TYPE) -> Optional[int]:

    # Get the conversation key
    invite_link = update.effective_message.text.removeprefix("/start_chat_with ")
    conversation_key = InviteLinkStore.get_conversation_key(invite_link)

    if conversation_key is None:
        await update.effective_message.reply_text("The invite link is not valid.")
        return ConversationHandler.END

    # Modify the conversation of the host, by changing its conversation key to the invite link
    host_conversation = await ConvHandlerDummy.conv_handler.get_conversation(conversation_key)
    host_conversation.state = START_CHAT
    host_conversation.update = update
    host_conversation.key = invite_link  # type: ignore
    if key_builder.player_store[update.effective_user.id]:
        await update.effective_message.reply_text(
            text="You can't start a conversation with yourself.",
            reply_markup=ReplyKeyboardRemove(),
        )
        return CHOOSE
    key_builder.player_store[update.effective_user.id] = invite_link
    await ConvHandlerDummy.conv_handler.update_conversation(
        conversation_key, host_conversation, application=context.application
    )
    await update.effective_message.reply_text(
        text="You successfully joined the chat. you can exit it with /cancel",
        reply_markup=ReplyKeyboardRemove(),
    )
    return None


async def start_chat(update: Update, context: ContextTypes.DEFAULT_TYPE) -> int:
    partner_ids = [
        key
        for key, value in key_builder.player_store.items()
        if value == context.conversation_data.key and key != update.effective_user.id
    ]
    if len(partner_ids) > 0:
        await context.bot.sendMessage(
            chat_id=partner_ids.pop(),
            text="A User has joined your conversation. You can exit it with /cancel",
            reply_markup=ReplyKeyboardRemove(),
        )
    return CHAT


async def chat(update: Update, context: ContextTypes.DEFAULT_TYPE) -> int:
    partner_id = next(
        key
        for key, value in key_builder.player_store.items()
        if value == context.conversation_data.key and key != update.effective_user.id
    )
    await update.effective_message.copy(partner_id)
    return CHAT


async def cancel(update: Update, context: ContextTypes.DEFAULT_TYPE) -> int:
    """Display the gathered info and end the conversation."""

    participants_ids = [
        key
        for key, value in key_builder.player_store.items()
        if value == context.conversation_data.key
    ]
    if len(participants_ids) > 0:
        for participant in participants_ids:
            key_builder.player_store.pop(participant)
            await context.bot.sendMessage(
                chat_id=participant,
                text="Conversation is ended. Good bye",
                reply_markup=ReplyKeyboardRemove(),
            )
    else:
        await update.effective_message.reply_text(
            text="Conversation is ended. Good bye",
            reply_markup=ReplyKeyboardRemove(),
        )

    return ConversationHandler.END


def main() -> None:
    """Run the bot."""
    # Create the Application and pass it your bot's token.
    application = Application.builder().token("TOKEN").build()

    # Add conversation handler with the states CHOOSING, TYPING_CHOICE and TYPING_REPLY
    ConvHandlerDummy.conv_handler = ConversationHandler(
        conversation_states=ConversationStates(
            entry_points=[CommandHandler("start", start)],
            state_entry_handlers={START_CHAT: [TypeHandler(Update, start_chat)]},
            states={
                CHOOSE: [
                    MessageHandler(
                        filters.Regex("^Host a chat$"),
                        host_chat,
                    ),
                    MessageHandler(
                        filters.Regex("^/start_chat_with "),
                        join_chat,
                    ),
                    MessageHandler(filters.Regex("^Exit$"), cancel),
                ],
                CHAT: [TypeHandler(Update, chat)],
            },
            pre_fallbacks=[MessageHandler(filters.Regex("^/cancel$"), cancel)],
        ),
        key_builder=key_builder,
    )

    application.add_handler(ConvHandlerDummy.conv_handler)

    # Run the bot until the user presses Ctrl-C
    application.run_polling(allowed_updates=Update.ALL_TYPES)


if __name__ == "__main__":
    main()

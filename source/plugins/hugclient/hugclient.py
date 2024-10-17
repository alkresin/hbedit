
from hugchat import hugchat
from hugchat.login import Login
import getpass
import argparse
import os
import traceback
import sys
sys.path.append('./')
from extsrv import esrv_Init, esrv_Wait, gWritelog

email = None
bot = None
stream_output = True
is_web_search = False
web_search_hint = False
continued_conv = False
hExt = None
gen_answer = None
conversation_dict = None

def handle_command(chatbot: any, aparams) -> None:
    global stream_output, is_web_search, web_search_hint , continued_conv
    global conversation_dict

    command = aparams[0][1:] # Remove the '/' at the start of the input
    arguments = aparams[1:]

    if command == "new":
        new_conversation = chatbot.new_conversation(switch_to=True)
        conversation_dict = None
        return "Created and switched to a new conversation\n# New conversation ID: {}".format(new_conversation.id)

    elif command == "switch":
        nNum = None
        try:
            if len(arguments) == 0:
                id = chatbot.get_conversation_list()
            elif arguments[0] == "all":
                id = chatbot.get_remote_conversations(replace_conversation_list=True)
            elif arguments[0].isdigit():
                nNum = int(arguments[0])
            else:
                return "Error"

            if nNum == None:
               conversation_dict = {i+1: id_string for i, id_string in enumerate(id)}

               cres = ""
               for i, id_string in conversation_dict.items():
                   info = chatbot.get_conversation_info(id_string)
                   cres += "{}: /{}/ {}\n".format(i, info.model[:24], info.title[:43].replace("\n"," ") )
               return cres

            elif nNum > 0 and conversation_dict != None:
               target_id = conversation_dict.get(nNum)

               if target_id:
                   chatbot.change_conversation(target_id)
                   info = chatbot.get_conversation_info()
                   return "Switched to: {} ({})".format(info.title[:43].replace("\n"," "), info.model )
               else:
                   return "Invalid number"
            else:
               return "Error"

        except Exception as e:
            return "Error: {}".format(e)

    elif command == "getcurrent":
        info = chatbot.get_conversation_info()
        try:
            cres = "{} ({})".format(info.title[:43].replace("\n"," "), info.model )
        except:
            cres = "No conversation active"
        return cres

    elif command == "gethistory":
        info = chatbot.get_conversation_info()
        try:
            cres = ""
            for i in info.history:
                cres += "{}\n".format(i)
        except:
            cres = "No conversation active"
        return cres

    elif command == "del":
        try:
            if not arguments:
                return "No argument for index provided."

            conversation_index = int(arguments[0])
            if conversation_index < 1:
                raise IndexError()
            selected_conversation = chatbot.get_conversation_list()[conversation_index-1].id
            current_conversation_id = chatbot.get_conversation_info().id

            if selected_conversation == current_conversation_id:
                return "Cannot delete active chat."

            to_delete_conversation = chatbot.get_conversation_from_id(selected_conversation)
        except Exception as e:
            if isinstance(e, ValueError):
                return "Invalid argument for index."
            else:
                return "Invalid index."

        if not to_delete_conversation is None:
            chatbot.delete_conversation(to_delete_conversation)
            return "Deleted conversation successfully."
        return "Error"

    elif command == "llm":
        if len(arguments) == 0:
            return "Available Models: {}".format([model.id for model in chatbot.get_available_llm_models()])

        try:
            chatbot.switch_llm(int(arguments[0]))
        except ValueError:
            return "Invalid LLM index"

        return "Switched to LLM {}\n Please note that you have to create a new conversation for this to take effect".format(chatbot.active_model.id)

    elif command == "sharewithauthor":
        sharing = arguments[0] == "on"
        chatbot.set_share_conversations(sharing)

        return "{}".format('Now sharing conversations with model author' if sharing else 'No longer sharing conversations with model author')

    elif command == "stream":
        stream_output = arguments[0] == "on"

        return "{}".format('Now streaming responses' if stream_output else 'No longer streaming responses')

    elif command == "web":
        is_web_search = arguments[0] == "on"

        return "{}".format('Web searching activated' if is_web_search else 'We searching deactivated')

    elif command == "web-hint":
        web_search_hint = arguments[0] == "on"

        return "{}".format('Enabled web hint' if web_search_hint else 'Disabled web hint')

    else:
        return "Unrecognized command"


def web_search(generator) -> None:
    s = ""
    sources = []
    for chunk in generator:
        if web_search_hint and chunk['type'] == 'webSearch' and chunk['messageType'] == 'update':
            args = chunk['args'][0] if 'args' in chunk else ""
            s += "Web Searching | {} {}".format(chunk['message'], args)

        elif web_search_hint and chunk['type'] == 'webSearch' and chunk['messageType'] == 'sources' and "sources" in chunk:
            sources = chunk['sources']

        elif chunk['type'] == 'stream':
            #print(chunk['token'], end="", flush=True)
            s += chunk['token']

    if web_search_hint and len(sources) > 0:
        s += "\n# Sources:"
        for i in range(len(sources)):
            s += "  {}. {} - {}".format(i+1, sources[i]['title'], sources[i]['link'] )

    return s

def loginToChat( cookies ):

    global bot
    bot = hugchat.ChatBot(cookies=cookies)
    #bot.switch_llm( 1 )
    #bot.new_conversation(switch_to=True)

def setemail( aparams ):

    global email
    email = aparams[0]
    try:
        cookies = Login(email).loadCookiesFromDir()
    except Exception:
        cookies = None

    if cookies is None:
        return "need psw"

    loginToChat( cookies )
    gWritelog(hExt, "Logged via cookie")
    return "Ok"

def setpass( aparams ):

    lOk = True
    try:
        gWritelog(hExt, "email: " + email)
        gWritelog(hExt, "pass: " + aparams[0])
        sign = Login(email, aparams[0])
        cookies = sign.login()
        sign.saveCookiesToDir()
        loginToChat( cookies )
        gWritelog(hExt, "Logged via password")
    except:
        gWritelog(hExt,  traceback.format_exc())
        return "Error"

    return "Ok"

def execcmd( aparams ):

    if aparams[0].startswith("/"):
        try:
            res = handle_command(bot, aparams)
        except:
            gWritelog(hExt, traceback.format_exc())
            return "Error"

    return res

def ask( aparams ):

    global gen_answer
    if is_web_search:
        generator = chatbot.chat(userInput, stream=True, _stream_yield_all=True, web_search=is_web_search)
        return web_search(generator)

    elif stream_output:
        gWritelog(hExt, "stream on")
        s = ""
        gen_answer = bot.chat(aparams[0])
        return "ok"
    else:
        gWritelog(hExt, "stream off")
        return bot.chat(aparams[0]).wait_until_done().strip()

def nexttoken( aparams ):
    global gen_answer
    n = aparams[0]
    s = ""
    while not gen_answer.is_done():
        chunk = next(gen_answer)
        if chunk is None:
            continue
        else:
            s += chunk["token"]
            n -= 1
            if n == 0:
                break
    return "====" if s == "" else s

def main( aparams ):

    global hExt
    hExt = esrv_Init(aparams)
    if hExt == None:
        return None

    gWritelog(hExt, "Init-Ok")
    esrv_Wait(hExt)

    return None

if __name__ == '__main__':
   os.chdir(os.path.dirname(os.path.realpath(sys.argv[0])))
   main( sys.argv[1:] )
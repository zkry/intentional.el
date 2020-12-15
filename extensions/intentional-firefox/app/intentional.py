#!/usr/bin/python3 -u

import json
import sys
import struct
import time

# Encode a message for transmission, given its content.
def encode_message():
    with open('/Users/YOUR_USER/browse-intentions.json', 'r') as file:
        data = file.read()
        #encoded_content = json.dumps(message_content).encode("utf-8")
        encoded_content = data.encode("utf-8")
        encoded_length = struct.pack('=I', len(encoded_content))
        #  use struct.pack("10s", bytes), to pack a string of the length of 10 characters
        return {'length': encoded_length, 'content': struct.pack(str(len(encoded_content))+'s',encoded_content)}

def send_message(encoded_message):
    sys.stdout.buffer.write(encoded_message['length'])
    sys.stdout.buffer.write(encoded_message['content'])
    sys.stdout.buffer.flush()

while True:
    send_message(encode_message())
    time.sleep(1)

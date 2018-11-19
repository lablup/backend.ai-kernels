#!/usr/bin/env python
from aiohttp import web
import aiohttp
import asyncio

async def websocket_handler(request):
    ws = web.WebSocketResponse(protocols=['tunnel-protocol',])
    await ws.prepare(request)
    try:
        reader, writer = await asyncio.open_connection('127.0.0.1', 8888)
    except ConnectionRefusedError as e:
        await ws.close(code=1014)
        return ws

    async def up():
        while True:
            chunk = await reader.read(8192)
            if not chunk:
                break
            await ws.send_bytes(chunk)

    asyncio.ensure_future(up())

    async for msg in ws:
        if msg.type == web.WSMsgType.binary:
            writer.write(msg.data)
            await writer.drain()

        elif msg.type == aiohttp.WSMsgType.ERROR:
            print('ws connection closed with exception %s' %
                  ws.exception())
            writer.close()
            await writer.wait_closed()

    print('websocket connection closed')

    return ws


def run():
    app = web.Application()
    app.add_routes([web.get('/', websocket_handler)])
    web.run_app(app, port=2002)(wstunnel)

if __name__=="__main__":
    run()

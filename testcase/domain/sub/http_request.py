import requests


def http_get(payload: dict):
    return requests.get(**payload)


def http_post(payload: dict):
    return requests.post(**payload)

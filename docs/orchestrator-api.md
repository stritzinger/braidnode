# Braidnet - Braidnode API

This is a [JSON-RPC 2.0](https://www.jsonrpc.org) API.

Example request-response pair:

```
--> {"jsonrpc": "2.0", "method": "some_method", "params": [1, 2], "id": 1}
```

then

```
<-- {"jsonrpc": "2.0", "result": 3, "id": 1}
```

or

```
<-- {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}
```

## Methods

### `start`

Start the application in the container.

#### **Parameters**

None

#### **Responses**

| result | description                      |
| ------ | -------------------------------- |
| `"ok"` | application successfully started |

#### **Error responses**

<!-- TODO: Separate error codes for common errors -->

| code | message                     | data (optional) |
| ---- | --------------------------- | --------------- |
| `-1` | `"Application not started"` | Reason          |

---

### `restart`

Restart the application in the container.

#### **Parameters**

None

#### **Responses**

| result | description                        |
| ------ | ---------------------------------- |
| `"ok"` | application successfully restarted |

---

### `stop`

Stop the application in the container.

#### **Parameters**

None

#### **Responses**

| result | description                      |
| ------ | -------------------------------- |
| `"ok"` | application successfully stopped |

---

### `ping`

Ping Braidnode or the application in the container.

#### **Parameters**

| key        | value                    | required |
| ---------- | ------------------------ | -------- |
| `"target"` | `"braidnode"` \| `"app"` | yes      |

#### **Responses**

| result   | description   |
| -------- | ------------- |
| `"pong"` | ping response |

---

### `register`

Register remote nodes in the container.

#### **Parameters**

| key       | value               | required |
| --------- | ------------------- | -------- |
| `"nodes"` | `[Node1, Node2...]` | yes      |

#### **Responses**

| result | description                   |
| ------ | ----------------------------- |
| `"ok"` | nodes successfully registered |

#### **Error responses**

| code | message                 | data                                      |
| ---- | ----------------------- | ----------------------------------------- |
| `-2` | `"Registration failed"` | `[{Node1: Reason1}, {Node2: Reason2}...]` |

---

### `deregister`

Deregister remote nodes in the container.
If no list of nodes is provided, all nodes are deregistered.

#### **Parameters**

| key       | value               | required |
| --------- | ------------------- | -------- |
| `"nodes"` | `[Node1, Node2...]` | no       |

#### **Responses**

| result | description                     |
| ------ | ------------------------------- |
| `"ok"` | nodes successfully deregistered |

#### **Error responses**

| code | message                   | data                                      |
| ---- | ------------------------- | ----------------------------------------- |
| `-3` | `"Deregistration failed"` | `[{Node1: Reason1}, {Node2: Reason2}...]` |

---

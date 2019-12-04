## POST /account/new-token

### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## POST /account/signin

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- some text, some text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signInUsername":"as such","signInPassword":"as such"}
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## POST /account/signup

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- some text, some text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signUpUsername":"as such","signUpPassword":"as such"}
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## GET /article/draft/:username/:article-name

### Captures:

- *username*: Username
- *article-name*: Article name

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"LatestDraftNotFound"}
```

## GET /article/draft/comments/:draft-key

### Captures:

- *draft-key*: Draft key

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- comment key, user key, some text (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"commentContent":"as such","commentAuthor":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8","commentID":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8"}]
```

- comment key, user key, some text, comment key, user key, some text (`application/json;charset=utf-8`):

```javascript
[{"commentContent":"as such","commentAuthor":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8","commentID":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8"},{"commentContent":"as such","commentAuthor":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8","commentID":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8"}]
```

## GET /article/draft/latest/:article-key

### Captures:

- *article-key*: Article key

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"LatestDraftNotFound"}
```

## POST /article/draft/link

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Linked"
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"LinkDraftFailure"
```

## POST /article/draft/new

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- article key, some text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"newDraftContents":"as such","newDraftArticle":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## POST /article/new

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an article name (`application/json;charset=utf-8`, `application/json`):

```javascript
"my brand new article"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- article key (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"ArticleCreated","contents":"b00e6dec-fcb4-4e4a-a298-c5cac17e37e8"}
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"ArticleCreationFailure"}
```



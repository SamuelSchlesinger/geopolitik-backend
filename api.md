## GET /

### Response:

- Status code 200
- Headers: []

- No response body

## POST /account/new-token

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>"),("Access-Control-Allow-Origin",""),("Access-Control-Allow-Headers",""),("Access-Control-Allow-Credentials","false")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text (`application/json;charset=utf-8`, `application/json`):

```javascript
"yes"
```

## POST /account/signin

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text, text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signInUsername":"yes","signInPassword":"yes"}
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>"),("Access-Control-Allow-Origin",""),("Access-Control-Allow-Headers",""),("Access-Control-Allow-Credentials","false")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text (`application/json;charset=utf-8`, `application/json`):

```javascript
"yes"
```

## POST /account/signup

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text, text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signUpUsername":"yes","signUpPassword":"yes"}
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

## POST /article/collaborator/add

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text, text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"addCollaboratorArticle":"yes","addCollaboratorUsername":"yes"}
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

## GET /article/draft/:username/:article-name

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Captures:

- *username*: Username
- *article-name*: Article name

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /article/draft/comments/:draft-key

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


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

## GET /article/draft/latest/:article-key

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Captures:

- *article-key*: Article key

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /article/draft/link

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text, an article tag, text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"linkDraftTag":"ArticleTag","linkDraftFrom":"yes","linkDraftTo":"yes"}
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

## POST /article/draft/new

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text, text (`application/json;charset=utf-8`, `application/json`):

```javascript
{"newDraftContents":"yes","newDraftArticle":"yes"}
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

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text (`application/json;charset=utf-8`, `application/json`):

```javascript
"yes"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- text (`application/json;charset=utf-8`, `application/json`):

```javascript
"yes"
```



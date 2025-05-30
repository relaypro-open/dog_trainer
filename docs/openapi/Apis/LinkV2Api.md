# LinkV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2LinkIdDelete**](LinkV2Api.md#apiV2LinkIdDelete) | **DELETE** /api/V2/link/{id} | Delete a Link by ID |
| [**apiV2LinkIdGet**](LinkV2Api.md#apiV2LinkIdGet) | **GET** /api/V2/link/{id} | Get a Link by ID |
| [**apiV2LinkIdHclGet**](LinkV2Api.md#apiV2LinkIdHclGet) | **GET** /api/V2/link/{id}/hcl | Get HCL representation of a Link |
| [**apiV2LinkIdPut**](LinkV2Api.md#apiV2LinkIdPut) | **PUT** /api/V2/link/{id} | Update a Link by ID |
| [**apiV2LinkPost**](LinkV2Api.md#apiV2LinkPost) | **POST** /api/V2/link | Create a new Link |
| [**apiV2LinksGet**](LinkV2Api.md#apiV2LinksGet) | **GET** /api/V2/links | List all Links |
| [**apiV2LinksSubGet**](LinkV2Api.md#apiV2LinksSubGet) | **GET** /api/V2/links/{sub} | Get a subset or filtered list of Links (e.g. &#39;schema&#39;) |


<a name="apiV2LinkIdDelete"></a>
# **apiV2LinkIdDelete**
> apiV2LinkIdDelete(id)

Delete a Link by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2LinkIdGet"></a>
# **apiV2LinkIdGet**
> Link apiV2LinkIdGet(id)

Get a Link by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**Link**](../Models/Link.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2LinkIdHclGet"></a>
# **apiV2LinkIdHclGet**
> String apiV2LinkIdHclGet(id)

Get HCL representation of a Link

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**String**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain, application/json

<a name="apiV2LinkIdPut"></a>
# **apiV2LinkIdPut**
> apiV2LinkIdPut(id, Link, inplace)

Update a Link by ID

    The backend &#x60;dog_link_api_v2:update/2&#x60; calls &#x60;reql:update&#x60;, implying a partial update is possible. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_link_api_v2:update/2&#x60; doesn&#39;t use it.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Link** | [**Link**](../Models/Link.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2LinkPost"></a>
# **apiV2LinkPost**
> Link apiV2LinkPost(Link)

Create a new Link

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Link** | [**Link**](../Models/Link.md)|  | |

### Return type

[**Link**](../Models/Link.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2LinksGet"></a>
# **apiV2LinksGet**
> List apiV2LinksGet()

List all Links

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Link.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2LinksSubGet"></a>
# **apiV2LinksSubGet**
> List apiV2LinksSubGet(sub)

Get a subset or filtered list of Links (e.g. &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/Link.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


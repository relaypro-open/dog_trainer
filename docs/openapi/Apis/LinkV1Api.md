# LinkV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiLinkIdDelete**](LinkV1Api.md#apiLinkIdDelete) | **DELETE** /api/link/{id} | Delete Link (V1) |
| [**apiLinkIdGet**](LinkV1Api.md#apiLinkIdGet) | **GET** /api/link/{id} | Get Link by ID (V1) |
| [**apiLinkIdPut**](LinkV1Api.md#apiLinkIdPut) | **PUT** /api/link/{id} | Update Link (V1) |
| [**apiLinkIdSubGet**](LinkV1Api.md#apiLinkIdSubGet) | **GET** /api/link/{id}/{sub} | Get Link sub-resource (V1) |
| [**apiLinkPost**](LinkV1Api.md#apiLinkPost) | **POST** /api/link | Create Link (V1) |
| [**apiLinksGet**](LinkV1Api.md#apiLinksGet) | **GET** /api/links | List Links (V1) |
| [**apiLinksSubGet**](LinkV1Api.md#apiLinksSubGet) | **GET** /api/links/{sub} | List Link sub-resources (V1) |


<a name="apiLinkIdDelete"></a>
# **apiLinkIdDelete**
> apiLinkIdDelete(id)

Delete Link (V1)

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

<a name="apiLinkIdGet"></a>
# **apiLinkIdGet**
> apiLinkIdGet(id)

Get Link by ID (V1)

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

<a name="apiLinkIdPut"></a>
# **apiLinkIdPut**
> apiLinkIdPut(id, Link)

Update Link (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Link** | [**Link**](../Models/Link.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiLinkIdSubGet"></a>
# **apiLinkIdSubGet**
> apiLinkIdSubGet(id, sub)

Get Link sub-resource (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiLinkPost"></a>
# **apiLinkPost**
> apiLinkPost(Link)

Create Link (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Link** | [**Link**](../Models/Link.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiLinksGet"></a>
# **apiLinksGet**
> apiLinksGet()

List Links (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiLinksSubGet"></a>
# **apiLinksSubGet**
> apiLinksSubGet(sub)

List Link sub-resources (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


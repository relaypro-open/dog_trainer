# ExternalV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiExternalIdDelete**](ExternalV1Api.md#apiExternalIdDelete) | **DELETE** /api/external/{id} | Delete External (V1) |
| [**apiExternalIdGet**](ExternalV1Api.md#apiExternalIdGet) | **GET** /api/external/{id} | Get External by ID (V1) |
| [**apiExternalIdPut**](ExternalV1Api.md#apiExternalIdPut) | **PUT** /api/external/{id} | Update External (V1) |
| [**apiExternalIdSubGet**](ExternalV1Api.md#apiExternalIdSubGet) | **GET** /api/external/{id}/{sub} | Get External sub-resource (V1) |
| [**apiExternalPost**](ExternalV1Api.md#apiExternalPost) | **POST** /api/external | Create External (V1) |
| [**apiExternalsGet**](ExternalV1Api.md#apiExternalsGet) | **GET** /api/externals | List Externals (V1) |
| [**apiExternalsSubGet**](ExternalV1Api.md#apiExternalsSubGet) | **GET** /api/externals/{sub} | List External sub-resources (V1) |


<a name="apiExternalIdDelete"></a>
# **apiExternalIdDelete**
> apiExternalIdDelete(id)

Delete External (V1)

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

<a name="apiExternalIdGet"></a>
# **apiExternalIdGet**
> apiExternalIdGet(id)

Get External by ID (V1)

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

<a name="apiExternalIdPut"></a>
# **apiExternalIdPut**
> apiExternalIdPut(id, External)

Update External (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **External** | [**External**](../Models/External.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiExternalIdSubGet"></a>
# **apiExternalIdSubGet**
> apiExternalIdSubGet(id, sub)

Get External sub-resource (V1)

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

<a name="apiExternalPost"></a>
# **apiExternalPost**
> apiExternalPost(External)

Create External (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **External** | [**External**](../Models/External.md)|  | |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiExternalsGet"></a>
# **apiExternalsGet**
> apiExternalsGet()

List Externals (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiExternalsSubGet"></a>
# **apiExternalsSubGet**
> apiExternalsSubGet(sub)

List External sub-resources (V1)

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


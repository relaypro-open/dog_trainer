# FactV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiFactIdDelete**](FactV1Api.md#apiFactIdDelete) | **DELETE** /api/fact/{id} | Delete Fact (V1) |
| [**apiFactIdGet**](FactV1Api.md#apiFactIdGet) | **GET** /api/fact/{id} | Get Fact by ID (V1) |
| [**apiFactIdPut**](FactV1Api.md#apiFactIdPut) | **PUT** /api/fact/{id} | Update Fact (V1) |
| [**apiFactIdSubGet**](FactV1Api.md#apiFactIdSubGet) | **GET** /api/fact/{id}/{sub} | Get Fact sub-resource (V1) |
| [**apiFactPost**](FactV1Api.md#apiFactPost) | **POST** /api/fact | Create Fact (V1) |
| [**apiFactsGet**](FactV1Api.md#apiFactsGet) | **GET** /api/facts | List Facts (V1) |
| [**apiFactsSubGet**](FactV1Api.md#apiFactsSubGet) | **GET** /api/facts/{sub} | List Fact sub-resources (V1) |


<a name="apiFactIdDelete"></a>
# **apiFactIdDelete**
> apiFactIdDelete(id)

Delete Fact (V1)

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

<a name="apiFactIdGet"></a>
# **apiFactIdGet**
> apiFactIdGet(id)

Get Fact by ID (V1)

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

<a name="apiFactIdPut"></a>
# **apiFactIdPut**
> apiFactIdPut(id, Fact)

Update Fact (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Fact** | [**Fact**](../Models/Fact.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiFactIdSubGet"></a>
# **apiFactIdSubGet**
> apiFactIdSubGet(id, sub)

Get Fact sub-resource (V1)

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

<a name="apiFactPost"></a>
# **apiFactPost**
> apiFactPost(Fact)

Create Fact (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Fact** | [**Fact**](../Models/Fact.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiFactsGet"></a>
# **apiFactsGet**
> apiFactsGet()

List Facts (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiFactsSubGet"></a>
# **apiFactsSubGet**
> apiFactsSubGet(sub)

List Fact sub-resources (V1)

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


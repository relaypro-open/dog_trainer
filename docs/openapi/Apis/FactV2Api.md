# FactV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2FactIdDelete**](FactV2Api.md#apiV2FactIdDelete) | **DELETE** /api/V2/fact/{id} | Delete a Fact by ID |
| [**apiV2FactIdGet**](FactV2Api.md#apiV2FactIdGet) | **GET** /api/V2/fact/{id} | Get a Fact by ID |
| [**apiV2FactIdHclGet**](FactV2Api.md#apiV2FactIdHclGet) | **GET** /api/V2/fact/{id}/hcl | Get HCL representation of a Fact |
| [**apiV2FactIdPut**](FactV2Api.md#apiV2FactIdPut) | **PUT** /api/V2/fact/{id} | Update a Fact by ID |
| [**apiV2FactPost**](FactV2Api.md#apiV2FactPost) | **POST** /api/V2/fact | Create a new Fact |
| [**apiV2FactsGet**](FactV2Api.md#apiV2FactsGet) | **GET** /api/V2/facts | List all Facts |
| [**apiV2FactsSubGet**](FactV2Api.md#apiV2FactsSubGet) | **GET** /api/V2/facts/{sub} | Get a subset or filtered list of Facts (e.g. &#39;schema&#39;) |


<a name="apiV2FactIdDelete"></a>
# **apiV2FactIdDelete**
> apiV2FactIdDelete(id)

Delete a Fact by ID

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

<a name="apiV2FactIdGet"></a>
# **apiV2FactIdGet**
> Fact apiV2FactIdGet(id)

Get a Fact by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**Fact**](../Models/Fact.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2FactIdHclGet"></a>
# **apiV2FactIdHclGet**
> String apiV2FactIdHclGet(id)

Get HCL representation of a Fact

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

<a name="apiV2FactIdPut"></a>
# **apiV2FactIdPut**
> apiV2FactIdPut(id, Fact)

Update a Fact by ID

    The backend &#x60;dog_fact_api_v2:update/2&#x60; calls &#x60;maps:merge&#x60; then &#x60;reql:replace&#x60;, implying a full replacement of the Fact resource.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Fact** | [**Fact**](../Models/Fact.md)|  | |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2FactPost"></a>
# **apiV2FactPost**
> Fact apiV2FactPost(Fact)

Create a new Fact

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Fact** | [**Fact**](../Models/Fact.md)|  | |

### Return type

[**Fact**](../Models/Fact.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2FactsGet"></a>
# **apiV2FactsGet**
> List apiV2FactsGet()

List all Facts

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Fact.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2FactsSubGet"></a>
# **apiV2FactsSubGet**
> List apiV2FactsSubGet(sub)

Get a subset or filtered list of Facts (e.g. &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/Fact.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


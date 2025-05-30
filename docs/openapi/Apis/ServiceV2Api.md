# ServiceV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2ServiceIdDelete**](ServiceV2Api.md#apiV2ServiceIdDelete) | **DELETE** /api/V2/service/{id} | Delete a Service by ID |
| [**apiV2ServiceIdGet**](ServiceV2Api.md#apiV2ServiceIdGet) | **GET** /api/V2/service/{id} | Get a Service by ID |
| [**apiV2ServiceIdHclGet**](ServiceV2Api.md#apiV2ServiceIdHclGet) | **GET** /api/V2/service/{id}/hcl | Get HCL representation of a Service |
| [**apiV2ServiceIdPut**](ServiceV2Api.md#apiV2ServiceIdPut) | **PUT** /api/V2/service/{id} | Update a Service by ID |
| [**apiV2ServicePost**](ServiceV2Api.md#apiV2ServicePost) | **POST** /api/V2/service | Create a new Service |
| [**apiV2ServicesGet**](ServiceV2Api.md#apiV2ServicesGet) | **GET** /api/V2/services | List all Services |
| [**apiV2ServicesSubGet**](ServiceV2Api.md#apiV2ServicesSubGet) | **GET** /api/V2/services/{sub} | Get a subset or filtered list of Services (e.g. &#39;schema&#39;) |


<a name="apiV2ServiceIdDelete"></a>
# **apiV2ServiceIdDelete**
> apiV2ServiceIdDelete(id, inplace)

Delete a Service by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ServiceIdGet"></a>
# **apiV2ServiceIdGet**
> Service apiV2ServiceIdGet(id, inplace)

Get a Service by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

[**Service**](../Models/Service.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ServiceIdHclGet"></a>
# **apiV2ServiceIdHclGet**
> String apiV2ServiceIdHclGet(id)

Get HCL representation of a Service

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

<a name="apiV2ServiceIdPut"></a>
# **apiV2ServiceIdPut**
> apiV2ServiceIdPut(id, Service, inplace)

Update a Service by ID

    The backend &#x60;dog_service_api_v2:update/2&#x60; calls &#x60;reql:update&#x60;, implying a partial update is possible. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_service_api_v2:update/2&#x60; doesn&#39;t use it.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Service** | [**Service**](../Models/Service.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ServicePost"></a>
# **apiV2ServicePost**
> Service apiV2ServicePost(Service)

Create a new Service

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Service** | [**Service**](../Models/Service.md)|  | |

### Return type

[**Service**](../Models/Service.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ServicesGet"></a>
# **apiV2ServicesGet**
> List apiV2ServicesGet()

List all Services

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Service.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ServicesSubGet"></a>
# **apiV2ServicesSubGet**
> List apiV2ServicesSubGet(sub)

Get a subset or filtered list of Services (e.g. &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/Service.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


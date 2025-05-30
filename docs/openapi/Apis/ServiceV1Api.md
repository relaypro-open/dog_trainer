# ServiceV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiServiceIdDelete**](ServiceV1Api.md#apiServiceIdDelete) | **DELETE** /api/service/{id} | Delete Service (V1) |
| [**apiServiceIdGet**](ServiceV1Api.md#apiServiceIdGet) | **GET** /api/service/{id} | Get Service by ID (V1) |
| [**apiServiceIdPut**](ServiceV1Api.md#apiServiceIdPut) | **PUT** /api/service/{id} | Update Service (V1) |
| [**apiServiceIdSubGet**](ServiceV1Api.md#apiServiceIdSubGet) | **GET** /api/service/{id}/{sub} | Get Service sub-resource (V1) |
| [**apiServicePost**](ServiceV1Api.md#apiServicePost) | **POST** /api/service | Create Service (V1) |
| [**apiServicesGet**](ServiceV1Api.md#apiServicesGet) | **GET** /api/services | List Services (V1) |
| [**apiServicesSubGet**](ServiceV1Api.md#apiServicesSubGet) | **GET** /api/services/{sub} | List Service sub-resources (V1) |


<a name="apiServiceIdDelete"></a>
# **apiServiceIdDelete**
> apiServiceIdDelete(id)

Delete Service (V1)

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

<a name="apiServiceIdGet"></a>
# **apiServiceIdGet**
> apiServiceIdGet(id)

Get Service by ID (V1)

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

<a name="apiServiceIdPut"></a>
# **apiServiceIdPut**
> apiServiceIdPut(id, Service)

Update Service (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Service** | [**Service**](../Models/Service.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiServiceIdSubGet"></a>
# **apiServiceIdSubGet**
> apiServiceIdSubGet(id, sub)

Get Service sub-resource (V1)

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

<a name="apiServicePost"></a>
# **apiServicePost**
> apiServicePost(Service)

Create Service (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Service** | [**Service**](../Models/Service.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiServicesGet"></a>
# **apiServicesGet**
> apiServicesGet()

List Services (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiServicesSubGet"></a>
# **apiServicesSubGet**
> apiServicesSubGet(sub)

List Service sub-resources (V1)

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


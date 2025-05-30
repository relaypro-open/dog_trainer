# ExternalV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2ExternalIdDelete**](ExternalV2Api.md#apiV2ExternalIdDelete) | **DELETE** /api/V2/external/{id} | Delete an External resource by ID |
| [**apiV2ExternalIdGet**](ExternalV2Api.md#apiV2ExternalIdGet) | **GET** /api/V2/external/{id} | Get an External resource by ID |
| [**apiV2ExternalIdHclGet**](ExternalV2Api.md#apiV2ExternalIdHclGet) | **GET** /api/V2/external/{id}/hcl | Get HCL representation of an External resource |
| [**apiV2ExternalIdPut**](ExternalV2Api.md#apiV2ExternalIdPut) | **PUT** /api/V2/external/{id} | Update/Replace an External resource by ID |
| [**apiV2ExternalPost**](ExternalV2Api.md#apiV2ExternalPost) | **POST** /api/V2/external | Create a new External resource |
| [**apiV2ExternalsGet**](ExternalV2Api.md#apiV2ExternalsGet) | **GET** /api/V2/externals | List all External resources |
| [**apiV2ExternalsSubGet**](ExternalV2Api.md#apiV2ExternalsSubGet) | **GET** /api/V2/externals/{sub} | Get a subset or filtered list of External resources (e.g., active) |


<a name="apiV2ExternalIdDelete"></a>
# **apiV2ExternalIdDelete**
> apiV2ExternalIdDelete(id)

Delete an External resource by ID

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

<a name="apiV2ExternalIdGet"></a>
# **apiV2ExternalIdGet**
> External apiV2ExternalIdGet(id)

Get an External resource by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**External**](../Models/External.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ExternalIdHclGet"></a>
# **apiV2ExternalIdHclGet**
> String apiV2ExternalIdHclGet(id)

Get HCL representation of an External resource

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

<a name="apiV2ExternalIdPut"></a>
# **apiV2ExternalIdPut**
> External apiV2ExternalIdPut(id, External)

Update/Replace an External resource by ID

    The backend &#x60;dog_external_api_v2:replace/2&#x60; function is called, which implies a full replacement rather than a partial update. The response code is typically 200 or 303 based on handler logic.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **External** | [**External**](../Models/External.md)|  | |

### Return type

[**External**](../Models/External.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ExternalPost"></a>
# **apiV2ExternalPost**
> External apiV2ExternalPost(External)

Create a new External resource

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **External** | [**External**](../Models/External.md)|  | |

### Return type

[**External**](../Models/External.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ExternalsGet"></a>
# **apiV2ExternalsGet**
> List apiV2ExternalsGet()

List all External resources

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/External.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ExternalsSubGet"></a>
# **apiV2ExternalsSubGet**
> List apiV2ExternalsSubGet(sub)

Get a subset or filtered list of External resources (e.g., active)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/External.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


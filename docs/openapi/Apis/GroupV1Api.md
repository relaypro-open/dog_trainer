# GroupV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiGroupIdDelete**](GroupV1Api.md#apiGroupIdDelete) | **DELETE** /api/group/{id} | Delete Group (V1) |
| [**apiGroupIdGet**](GroupV1Api.md#apiGroupIdGet) | **GET** /api/group/{id} | Get Group by ID (V1) |
| [**apiGroupIdPut**](GroupV1Api.md#apiGroupIdPut) | **PUT** /api/group/{id} | Update Group (V1) |
| [**apiGroupIdSubGet**](GroupV1Api.md#apiGroupIdSubGet) | **GET** /api/group/{id}/{sub} | Get Group sub-resource (V1) |
| [**apiGroupPost**](GroupV1Api.md#apiGroupPost) | **POST** /api/group | Create Group (V1) |
| [**apiGroupsGet**](GroupV1Api.md#apiGroupsGet) | **GET** /api/groups | List Groups (V1) |
| [**apiGroupsSubGet**](GroupV1Api.md#apiGroupsSubGet) | **GET** /api/groups/{sub} | List Group sub-resources (V1) |


<a name="apiGroupIdDelete"></a>
# **apiGroupIdDelete**
> apiGroupIdDelete(id)

Delete Group (V1)

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

<a name="apiGroupIdGet"></a>
# **apiGroupIdGet**
> apiGroupIdGet(id)

Get Group by ID (V1)

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

<a name="apiGroupIdPut"></a>
# **apiGroupIdPut**
> apiGroupIdPut(id, Group)

Update Group (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Group** | [**Group**](../Models/Group.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiGroupIdSubGet"></a>
# **apiGroupIdSubGet**
> apiGroupIdSubGet(id, sub)

Get Group sub-resource (V1)

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

<a name="apiGroupPost"></a>
# **apiGroupPost**
> apiGroupPost(Group)

Create Group (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Group** | [**Group**](../Models/Group.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiGroupsGet"></a>
# **apiGroupsGet**
> apiGroupsGet()

List Groups (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiGroupsSubGet"></a>
# **apiGroupsSubGet**
> apiGroupsSubGet(sub)

List Group sub-resources (V1)

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


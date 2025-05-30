# ProfileV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiProfileIdDelete**](ProfileV1Api.md#apiProfileIdDelete) | **DELETE** /api/profile/{id} | Delete Profile (V1) |
| [**apiProfileIdGet**](ProfileV1Api.md#apiProfileIdGet) | **GET** /api/profile/{id} | Get Profile by ID (V1) |
| [**apiProfileIdPut**](ProfileV1Api.md#apiProfileIdPut) | **PUT** /api/profile/{id} | Update Profile (V1) |
| [**apiProfileIdSubGet**](ProfileV1Api.md#apiProfileIdSubGet) | **GET** /api/profile/{id}/{sub} | Get Profile sub-resource (V1) |
| [**apiProfilePost**](ProfileV1Api.md#apiProfilePost) | **POST** /api/profile | Create Profile (V1) |
| [**apiProfilesGet**](ProfileV1Api.md#apiProfilesGet) | **GET** /api/profiles | List Profiles (V1) |
| [**apiProfilesSubGet**](ProfileV1Api.md#apiProfilesSubGet) | **GET** /api/profiles/{sub} | List Profile sub-resources (V1) |


<a name="apiProfileIdDelete"></a>
# **apiProfileIdDelete**
> apiProfileIdDelete(id)

Delete Profile (V1)

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

<a name="apiProfileIdGet"></a>
# **apiProfileIdGet**
> apiProfileIdGet(id)

Get Profile by ID (V1)

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

<a name="apiProfileIdPut"></a>
# **apiProfileIdPut**
> apiProfileIdPut(id, Profile)

Update Profile (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Profile** | [**Profile**](../Models/Profile.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiProfileIdSubGet"></a>
# **apiProfileIdSubGet**
> apiProfileIdSubGet(id, sub)

Get Profile sub-resource (V1)

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

<a name="apiProfilePost"></a>
# **apiProfilePost**
> apiProfilePost(Profile)

Create Profile (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Profile** | [**Profile**](../Models/Profile.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiProfilesGet"></a>
# **apiProfilesGet**
> apiProfilesGet()

List Profiles (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiProfilesSubGet"></a>
# **apiProfilesSubGet**
> apiProfilesSubGet(sub)

List Profile sub-resources (V1)

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


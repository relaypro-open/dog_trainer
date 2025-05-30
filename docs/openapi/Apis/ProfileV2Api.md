# ProfileV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2ProfileIdDelete**](ProfileV2Api.md#apiV2ProfileIdDelete) | **DELETE** /api/V2/profile/{id} | Delete a Profile by ID |
| [**apiV2ProfileIdGet**](ProfileV2Api.md#apiV2ProfileIdGet) | **GET** /api/V2/profile/{id} | Get a Profile by ID |
| [**apiV2ProfileIdHclGet**](ProfileV2Api.md#apiV2ProfileIdHclGet) | **GET** /api/V2/profile/{id}/hcl | Get HCL representation of a Profile |
| [**apiV2ProfileIdPut**](ProfileV2Api.md#apiV2ProfileIdPut) | **PUT** /api/V2/profile/{id} | Update a Profile by ID |
| [**apiV2ProfilePost**](ProfileV2Api.md#apiV2ProfilePost) | **POST** /api/V2/profile | Create a new Profile |
| [**apiV2ProfilesGet**](ProfileV2Api.md#apiV2ProfilesGet) | **GET** /api/V2/profiles | List all Profiles |
| [**apiV2ProfilesSubGet**](ProfileV2Api.md#apiV2ProfilesSubGet) | **GET** /api/V2/profiles/{sub} | Get a subset of Profiles (e.g., &#39;active&#39;, &#39;names&#39;, &#39;schema&#39;) |


<a name="apiV2ProfileIdDelete"></a>
# **apiV2ProfileIdDelete**
> apiV2ProfileIdDelete(id)

Delete a Profile by ID

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

<a name="apiV2ProfileIdGet"></a>
# **apiV2ProfileIdGet**
> Profile apiV2ProfileIdGet(id)

Get a Profile by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**Profile**](../Models/Profile.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ProfileIdHclGet"></a>
# **apiV2ProfileIdHclGet**
> String apiV2ProfileIdHclGet(id)

Get HCL representation of a Profile

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

<a name="apiV2ProfileIdPut"></a>
# **apiV2ProfileIdPut**
> apiV2ProfileIdPut(id, Profile, inplace)

Update a Profile by ID

    The backend &#x60;dog_profile_api_v2:update/2&#x60; calls &#x60;reql:update&#x60;, implying a partial update is possible. The &#x60;inplace&#x60; parameter is handled by &#x60;api_handler_v2&#x60; and passed to &#x60;dog_profile_api_v2:update/3&#x60;.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Profile** | [**Profile**](../Models/Profile.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ProfilePost"></a>
# **apiV2ProfilePost**
> Profile apiV2ProfilePost(Profile)

Create a new Profile

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Profile** | [**Profile**](../Models/Profile.md)|  | |

### Return type

[**Profile**](../Models/Profile.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ProfilesGet"></a>
# **apiV2ProfilesGet**
> List apiV2ProfilesGet()

List all Profiles

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Profile.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ProfilesSubGet"></a>
# **apiV2ProfilesSubGet**
> List apiV2ProfilesSubGet(sub)

Get a subset of Profiles (e.g., &#39;active&#39;, &#39;names&#39;, &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/_api_V2_profiles__sub__get_200_response_inner.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


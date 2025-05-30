# HostV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2HostIdDelete**](HostV2Api.md#apiV2HostIdDelete) | **DELETE** /api/V2/host/{id} | Delete a Host by ID |
| [**apiV2HostIdGet**](HostV2Api.md#apiV2HostIdGet) | **GET** /api/V2/host/{id} | Get a Host by ID |
| [**apiV2HostIdHclGet**](HostV2Api.md#apiV2HostIdHclGet) | **GET** /api/V2/host/{id}/hcl | Get HCL representation of a Host |
| [**apiV2HostIdPut**](HostV2Api.md#apiV2HostIdPut) | **PUT** /api/V2/host/{id} | Update a Host by ID |
| [**apiV2HostPost**](HostV2Api.md#apiV2HostPost) | **POST** /api/V2/host | Create a new Host |
| [**apiV2HostsGet**](HostV2Api.md#apiV2HostsGet) | **GET** /api/V2/hosts | List all Hosts |
| [**apiV2HostsIpsGet**](HostV2Api.md#apiV2HostsIpsGet) | **GET** /api/V2/hosts/ips | Get all IPs for all active hosts (special plural handler case) |
| [**apiV2HostsSubGet**](HostV2Api.md#apiV2HostsSubGet) | **GET** /api/V2/hosts/{sub} | Get a subset of Hosts (e.g., &#39;active&#39;, &#39;ips&#39;, &#39;names&#39;, &#39;hostkeys&#39;, &#39;schema&#39;) |


<a name="apiV2HostIdDelete"></a>
# **apiV2HostIdDelete**
> apiV2HostIdDelete(id)

Delete a Host by ID

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

<a name="apiV2HostIdGet"></a>
# **apiV2HostIdGet**
> Host apiV2HostIdGet(id)

Get a Host by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**Host**](../Models/Host.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2HostIdHclGet"></a>
# **apiV2HostIdHclGet**
> String apiV2HostIdHclGet(id)

Get HCL representation of a Host

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

<a name="apiV2HostIdPut"></a>
# **apiV2HostIdPut**
> apiV2HostIdPut(id, Host, inplace)

Update a Host by ID

    The backend &#x60;dog_host_api_v2:update/2&#x60; calls &#x60;reql:replace&#x60;, implying a full replacement. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_host_api_v2:update/2&#x60; doesn&#39;t use it.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Host** | [**Host**](../Models/Host.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2HostPost"></a>
# **apiV2HostPost**
> Host apiV2HostPost(Host)

Create a new Host

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Host** | [**Host**](../Models/Host.md)|  | |

### Return type

[**Host**](../Models/Host.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2HostsGet"></a>
# **apiV2HostsGet**
> List apiV2HostsGet(name, hostkey)

List all Hosts

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **name** | **String**| Filter by resource name | [optional] [default to null] |
| **hostkey** | **String**| Filter by host key (for Host resources) | [optional] [default to null] |

### Return type

[**List**](../Models/Host.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2HostsIpsGet"></a>
# **apiV2HostsIpsGet**
> Map apiV2HostsIpsGet()

Get all IPs for all active hosts (special plural handler case)

### Parameters
This endpoint does not need any parameter.

### Return type

[**Map**](../Models/array.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2HostsSubGet"></a>
# **apiV2HostsSubGet**
> List apiV2HostsSubGet(sub)

Get a subset of Hosts (e.g., &#39;active&#39;, &#39;ips&#39;, &#39;names&#39;, &#39;hostkeys&#39;, &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/_api_V2_hosts__sub__get_200_response_inner.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


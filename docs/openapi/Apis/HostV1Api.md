# HostV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiHostIdDelete**](HostV1Api.md#apiHostIdDelete) | **DELETE** /api/host/{id} | Delete Host (V1) |
| [**apiHostIdGet**](HostV1Api.md#apiHostIdGet) | **GET** /api/host/{id} | Get Host by ID (V1) |
| [**apiHostIdPut**](HostV1Api.md#apiHostIdPut) | **PUT** /api/host/{id} | Update Host (V1) |
| [**apiHostIdSubGet**](HostV1Api.md#apiHostIdSubGet) | **GET** /api/host/{id}/{sub} | Get Host sub-resource (V1) |
| [**apiHostPost**](HostV1Api.md#apiHostPost) | **POST** /api/host | Create Host (V1) |
| [**apiHostsGet**](HostV1Api.md#apiHostsGet) | **GET** /api/hosts | List Hosts (V1) |
| [**apiHostsIpsGet**](HostV1Api.md#apiHostsIpsGet) | **GET** /api/hosts/ips | List Host IPs (V1) |
| [**apiHostsSubGet**](HostV1Api.md#apiHostsSubGet) | **GET** /api/hosts/{sub} | List Host sub-resources (V1, e.g. &#39;ips&#39;) |


<a name="apiHostIdDelete"></a>
# **apiHostIdDelete**
> apiHostIdDelete(id)

Delete Host (V1)

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

<a name="apiHostIdGet"></a>
# **apiHostIdGet**
> apiHostIdGet(id)

Get Host by ID (V1)

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

<a name="apiHostIdPut"></a>
# **apiHostIdPut**
> apiHostIdPut(id, Host)

Update Host (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Host** | [**Host**](../Models/Host.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiHostIdSubGet"></a>
# **apiHostIdSubGet**
> apiHostIdSubGet(id, sub)

Get Host sub-resource (V1)

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

<a name="apiHostPost"></a>
# **apiHostPost**
> apiHostPost(Host)

Create Host (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Host** | [**Host**](../Models/Host.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiHostsGet"></a>
# **apiHostsGet**
> apiHostsGet()

List Hosts (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiHostsIpsGet"></a>
# **apiHostsIpsGet**
> apiHostsIpsGet()

List Host IPs (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiHostsSubGet"></a>
# **apiHostsSubGet**
> apiHostsSubGet(sub)

List Host sub-resources (V1, e.g. &#39;ips&#39;)

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


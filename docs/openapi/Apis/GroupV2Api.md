# GroupV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2GroupIdDelete**](GroupV2Api.md#apiV2GroupIdDelete) | **DELETE** /api/V2/group/{id} | Delete a Group by ID |
| [**apiV2GroupIdEc2SecurityGroupIdsGet**](GroupV2Api.md#apiV2GroupIdEc2SecurityGroupIdsGet) | **GET** /api/V2/group/{id}/ec2_security_group_ids | Get EC2 security group IDs for a Group |
| [**apiV2GroupIdExternalIpsGet**](GroupV2Api.md#apiV2GroupIdExternalIpsGet) | **GET** /api/V2/group/{id}/external_ips | Get external IP addresses for a Group |
| [**apiV2GroupIdGet**](GroupV2Api.md#apiV2GroupIdGet) | **GET** /api/V2/group/{id} | Get a Group by ID |
| [**apiV2GroupIdHclGet**](GroupV2Api.md#apiV2GroupIdHclGet) | **GET** /api/V2/group/{id}/hcl | Get HCL representation of a Group |
| [**apiV2GroupIdHostsGet**](GroupV2Api.md#apiV2GroupIdHostsGet) | **GET** /api/V2/group/{id}/hosts | Get all Hosts within a Group |
| [**apiV2GroupIdInternalIpsGet**](GroupV2Api.md#apiV2GroupIdInternalIpsGet) | **GET** /api/V2/group/{id}/internal_ips | Get internal IP addresses for a Group |
| [**apiV2GroupIdIpsGet**](GroupV2Api.md#apiV2GroupIdIpsGet) | **GET** /api/V2/group/{id}/ips | Get all IP addresses for a Group |
| [**apiV2GroupIdIpv4sGet**](GroupV2Api.md#apiV2GroupIdIpv4sGet) | **GET** /api/V2/group/{id}/ipv4s | Get all IPv4 addresses for a Group |
| [**apiV2GroupIdIpv6sGet**](GroupV2Api.md#apiV2GroupIdIpv6sGet) | **GET** /api/V2/group/{id}/ipv6s | Get all IPv6 addresses for a Group |
| [**apiV2GroupIdPut**](GroupV2Api.md#apiV2GroupIdPut) | **PUT** /api/V2/group/{id} | Update a Group by ID |
| [**apiV2GroupPost**](GroupV2Api.md#apiV2GroupPost) | **POST** /api/V2/group | Create a new Group |
| [**apiV2GroupsGet**](GroupV2Api.md#apiV2GroupsGet) | **GET** /api/V2/groups | List all Groups |
| [**apiV2GroupsSubGet**](GroupV2Api.md#apiV2GroupsSubGet) | **GET** /api/V2/groups/{sub} | Get a subset of Groups (e.g., &#39;active&#39;, &#39;all_active_names&#39;, &#39;names&#39;, &#39;schema&#39;) |


<a name="apiV2GroupIdDelete"></a>
# **apiV2GroupIdDelete**
> apiV2GroupIdDelete(id)

Delete a Group by ID

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

<a name="apiV2GroupIdEc2SecurityGroupIdsGet"></a>
# **apiV2GroupIdEc2SecurityGroupIdsGet**
> List apiV2GroupIdEc2SecurityGroupIdsGet(id)

Get EC2 security group IDs for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**List**](../Models/Ec2SecurityGroup.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdExternalIpsGet"></a>
# **apiV2GroupIdExternalIpsGet**
> List apiV2GroupIdExternalIpsGet(id)

Get external IP addresses for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**List**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdGet"></a>
# **apiV2GroupIdGet**
> Group apiV2GroupIdGet(id)

Get a Group by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**Group**](../Models/Group.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdHclGet"></a>
# **apiV2GroupIdHclGet**
> String apiV2GroupIdHclGet(id)

Get HCL representation of a Group

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

<a name="apiV2GroupIdHostsGet"></a>
# **apiV2GroupIdHostsGet**
> List apiV2GroupIdHostsGet(id)

Get all Hosts within a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

[**List**](../Models/Host.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdInternalIpsGet"></a>
# **apiV2GroupIdInternalIpsGet**
> List apiV2GroupIdInternalIpsGet(id)

Get internal IP addresses for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**List**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdIpsGet"></a>
# **apiV2GroupIdIpsGet**
> List apiV2GroupIdIpsGet(id)

Get all IP addresses for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**List**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdIpv4sGet"></a>
# **apiV2GroupIdIpv4sGet**
> List apiV2GroupIdIpv4sGet(id)

Get all IPv4 addresses for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**List**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdIpv6sGet"></a>
# **apiV2GroupIdIpv6sGet**
> List apiV2GroupIdIpv6sGet(id)

Get all IPv6 addresses for a Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**List**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupIdPut"></a>
# **apiV2GroupIdPut**
> apiV2GroupIdPut(id, Group, inplace)

Update a Group by ID

    The backend &#x60;dog_group_api_v2:update/2&#x60; calls &#x60;reql:replace&#x60;, implying a full replacement. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_group_api_v2:update/2&#x60; doesn&#39;t use it, it calls &#x60;dog_group_api_v2:update/3&#x60; which also doesn&#39;t use &#x60;InPlace&#x60; for group updates.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Group** | [**Group**](../Models/Group.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2GroupPost"></a>
# **apiV2GroupPost**
> Group apiV2GroupPost(Group)

Create a new Group

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Group** | [**Group**](../Models/Group.md)|  | |

### Return type

[**Group**](../Models/Group.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2GroupsGet"></a>
# **apiV2GroupsGet**
> List apiV2GroupsGet()

List all Groups

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Group.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2GroupsSubGet"></a>
# **apiV2GroupsSubGet**
> List apiV2GroupsSubGet(sub)

Get a subset of Groups (e.g., &#39;active&#39;, &#39;all_active_names&#39;, &#39;names&#39;, &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/_api_V2_groups__sub__get_200_response_inner.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain


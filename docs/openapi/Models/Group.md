# Group
## Properties

| Name | Type | Description | Notes |
|------------ | ------------- | ------------- | -------------|
| **name** | **String** |  | [default to null] |
| **profile\_name** | **String** |  | [optional] [default to null] |
| **ec2\_security\_group\_ids** | [**List**](Ec2SecurityGroup.md) |  | [optional] [default to null] |
| **profile\_id** | **String** | ID of the associated profile. Set by backend based on profile_name. | [optional] [default to null] |
| **vars** | [**Map**](AnyType.md) | Key-value pairs for group variables | [optional] [default to null] |
| **created** | **Date** | Timestamp of creation, set by backend. | [optional] [default to null] |
| **hash4\_ipsets** | **String** |  | [optional] [default to ] |
| **hash6\_ipsets** | **String** |  | [optional] [default to ] |
| **hash4\_iptables** | **String** |  | [optional] [default to ] |
| **hash6\_iptables** | **String** |  | [optional] [default to ] |
| **ipset\_hash** | **String** |  | [optional] [default to ] |
| **external\_ipv4\_addresses** | **List** |  | [optional] [default to []] |
| **external\_ipv6\_addresses** | **List** |  | [optional] [default to []] |
| **profile\_version** | **String** |  | [optional] [default to latest] |

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


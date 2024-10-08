Useful Reql Queries
--

Agent Versions:

```
r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).group("version").getField("name")
```

Group by ipset_hash:

```
r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).group("ipset_hash").getField("name")
```

True/false if all hosts are unique:

```
r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).count().eq(
  r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).distinct().count())
```

Make sure no hostkey matches 'localhost':

```
r.db('dog').table('host').orderBy("name")
  .filter( 
    r.row("active").eq("active")
  ).filter(
    r.row("hostkey").match("localhost")
  ).pluck(["name","hostkey"])
```

Active agents not passing keepalive check:

```
r.db('dog').table('host')
  .filter(
    r.row("keepalive_timestamp").lt(r.now().toEpochTime().sub(120)) 
    .and ( 
    r.row("active").eq("active"))
  )
  .pluck(["name","keepalive_timestamp"])
```

Agents Not Matching Global Ipset Hash:

```
r.db("dog").table("host")
  .filter(
    r.row("active").eq("active"))
  .filter( 
    r.row("ipset_hash").ne(r.db("dog").table("ipset").getAll("global",{index:'name'}).nth(0).getField("hash")))
```

```
r.db("dog").table("host")
  .filter(
    r.row("active").eq("active"))
  .filter( 
    r.row("ipset_hash")
    .ne(r.db("dog").table("ipset").orderBy(r.desc("timestamp")).limit(1)
      .getField("hash")))
```

All external ipv4 addresss:

```
r.db("dog").table("group").getField("external_ipv4_addresses").concatMap(function(x) { return x }).coerceTo('array').distinct()
```

Find groups by regex name:

```
r.db('dog').table('group')
  .filter( 
    r.row("name").match("dog"))
```

Duplicate hostkeys in host records:

```
r.db('dog').table('host')
 .filter(r.row("active").eq("active"))
 .group('hostkey').count().ungroup()
 .filter(row => row('reduction').gt(1))('group')
```

Duplicate hostkeys in host records:

```
r.db('dog').table('host')
 .group('hostkey').count().ungroup()
 .filter(row => row('reduction').gt(1))('group')
```

Find host by IP:

```
r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).filter(
    r.row("interfaces").match("1\.1\.1\.1")
  )
```

Find profile by text in rules:

```
r.db('dog').table('profile')
  .filter(
    r.row("rules").toJSON().match("test_zone")
  )
```

Find address in zones:

```
r.db('dog').table('zone')
  .filter(
    r.row("ipv4_addresses").toJSON().match("1.2.3.4")
  )
```

List all secondary indexes in all tables:

```
r.db("dog").tableList().map(
  function(table) {
  	return [table, r.db("dog").table(table).indexList()]
      }
  )
```

groups in profile rules:

```
r.db('dog').table('profile')('rules')('inbound').map(function (rule) {
  return rule('group')})
  ```

All hosts who's ipv4 hash doesn't match their group's ipv4 hash:

```
r.db('dog').table('host')
  .filter(r.row("active").eq("active"))
  .eqJoin('group', r.db('dog').table('group'), {index: 'name'})
  .filter(r.row("right")("hash4_ipsets").ne(""))
  .filter(r.row("left")("hash4_ipsets").ne(r.row("right")("hash4_ipsets")))
  //('left')('hostkey')
  .pluck([{'left' : ['name', 'hash4_ipsets','version']},{'right' : ['name', 'hash4_ipsets']}])
```

List Host ec2_security_groups by associated Groups:

```
r.db('dog').table('host')
  .filter(r.row("active").eq("active"))
  .eqJoin('group', r.db('dog').table('group'), {index: 'name'})
  .zip().group("name").pluck(["ec2_security_group_ids","ec2_availability_zone"]).distinct()
```

ec2 SG info by group without ec2_security_group_ids defined, useful to figure why they are not defined:

```
r.db('dog').table('host')
  .filter(r.row("active").eq("active"))
  .eqJoin('group', r.db('dog').table('group'), {index: 'name'})
  .filter(
  r.row("right").hasFields('ec2_security_group_ids').not()
  )
  .group(function(host) {
    return host('right')('name')
  })
  .pluck([{"left": ["ec2_security_group_ids","name"]}])
```

zones by count of IPs:

```
r.db('dog').table('zone').map
   (function(zone) {
     return {count: zone('ipv4_addresses').count(),name: zone('name')}
   }).group("count")
```

list non-existent groups in active profiles (pathological case) (ignore 'all-active'):

```
r.db('dog').table('group').withFields(['profile_id']).innerJoin(
  r.db('dog').table('profile'), 
  function(groupRow, profileRow) {
    return groupRow('profile_id').eq(profileRow('id'))})('right')('rules')('inbound')
  .concatMap(function(rule) {
    return rule.pluck(["group","group_type"])
  })
  .filter(function(rule) {
    return rule('group_type').eq('ROLE')
  }).getField('group').distinct().setDifference(r.db('dog').table('group')('id').distinct())
```

list non-existent zones in profiles (pathological case) (ignore 'cpz_any'):
```
  r.db('dog').table('profile')('rules')('inbound')              
  .concatMap(function(rule) {                                                                  
    return rule                                           
  })
    .filter(function(rule) {
      return rule('group_type').eq('ZONE')
    })
    .getField('group').distinct().setDifference(r.db('dog').table('zone')('id').distinct())
```

list non-existent services in active profiles (pathological case) (ignore 'any'):
```
r.db('dog').table('group').withFields(['profile_id']).innerJoin(                            
  r.db('dog').table('profile'),                                                             
  function(serviceRow, profileRow) {                                                          
    return serviceRow('profile_id').eq(profileRow('id'))})('right')('rules')('inbound')       
  .concatMap(function(rule) {                                                               
    return rule.pluck(["service"])                                               
  })                        
  .getField('service').distinct().setDifference(r.db('dog').table('service')('id').distinct())
```

query by ec2 tags:
```
r.db('dog').table('host')
  .filter(
      {'ec2_instance_tags': 
        {
          'environment':'qa',
          'cluster':'x'
        }
      }
    )
  .filter( 
    r.row("active").eq("active")
   )
  .count()
```

group by ec2 tags:
```
r.db('dog').table('host')
    .filter( 
    r.row("active").eq("active")
  )
  .hasFields({ ec2_instance_tags: 'role' })('ec2_instance_tags')
  .group('environment','cluster').count()
```

hosts without field
```
r.db('dog').table('host')
  .filter(
    {active:"active"}
    )
  .filter( 
     r.row.hasFields('ec2_instance_tags').not()
  )('name')
```

hosts by OS and version
```
r.db('dog').table('host')
  .filter( 
    r.row("active").eq("active")
  ).group("os_version","os_distribution").count()
```

migrate rules from profile to ruleset tables:
```
r.db('dog').table('profile').forEach(function(profile) {
  return r.db('dog').table('ruleset').insert(
    {"name": profile("name"), "rules": profile("rules"), "profile_id": profile("id")})
})

r.db('dog').table('profile').replace(function(elem) {
  return elem.without('rules')
})
```

roll back rules from ruleset back to profile:
```
r.db('dog').table('ruleset').forEach(function(ruleset) {
  return r.db('dog').table('profile').get(ruleset('profile_id')).update(
    {"rules": ruleset("rules")})
})
```

find IP in zones:
```
r.db('dog').table('zone')
  .filter(
    r.row("ipv4_addresses").toJSON().match("1.1.1.1")
  )
```

Active ruleset IDs:
```
r.db('dog').table('group').withFields(['profile_id']).innerJoin(
  r.db('dog').table('ruleset').hasFields(['profile_id']),
  function(groupRow, rulesetRow) {
    return groupRow('profile_id').eq(rulesetRow('profile_id'))})('right')('id')
```

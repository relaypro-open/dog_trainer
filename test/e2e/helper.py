import sys
import time
import json
import urllib.request
import urllib.error

def api_get(url, token):
    req = urllib.request.Request(url)
    req.add_header("Authorization", f"Bearer {token}")
    try:
        with urllib.request.urlopen(req, timeout=5) as response:
            if response.status == 200:
                return json.loads(response.read().decode())
    except Exception:
        pass
    return None

def wait_for_host_registered(base_url, token, hostname, timeout):
    start_time = time.time()
    url = f"{base_url}/V2/host?name={hostname}"
    while time.time() - start_time < timeout:
        data = api_get(url, token)
        if data:
            if isinstance(data, list) and len(data) > 0:
                return data[0]["id"]
            elif isinstance(data, dict) and "id" in data:
                return data["id"]
        time.sleep(2)
    return None

def wait_for_convergence(base_url, token, group_id, host_id, timeout):
    start_time = time.time()
    g_url = f"{base_url}/V2/group/{group_id}"
    h_url = f"{base_url}/V2/host/{host_id}"
    while time.time() - start_time < timeout:
        g = api_get(g_url, token)
        h = api_get(h_url, token)
        if g and h:
            g_ipt = g.get("hash4_iptables")
            g_ips = g.get("hash4_ipsets")
            h_ipt = h.get("hash4_iptables")
            h_ips = h.get("hash4_ipsets")
            if g_ipt and g_ipt == h_ipt and g_ips and g_ips == h_ips:
                return True
        time.sleep(2)
    return False

if __name__ == "__main__":
    if len(sys.argv) < 4:
        sys.exit(1)
    cmd = sys.argv[1]
    base_url = sys.argv[2]
    token = sys.argv[3]
    if cmd == "wait_host":
        hostname = sys.argv[4]
        timeout = int(sys.argv[5])
        res = wait_for_host_registered(base_url, token, hostname, timeout)
        if res:
            print(res)
            sys.exit(0)
        sys.exit(1)
    elif cmd == "wait_convergence":
        group_id = sys.argv[4]
        host_id = sys.argv[5]
        timeout = int(sys.argv[6])
        if wait_for_convergence(base_url, token, group_id, host_id, timeout):
            print("Convergence reached!")
            sys.exit(0)
        sys.exit(1)

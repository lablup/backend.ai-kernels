import os
import json
from pathlib import Path


def get_ssh_port(mapping, cluster_host):
    if mapping is None:
        return 2220
    try:
        _, port = mapping[cluster_host]  # host-ip and port
        return port
    except KeyError:
        return 2220


if __name__ == "__main__":
    if "BACKENDAI_CLUSTER_HOST" in os.environ: # Start mutli-instance setup.
        try:
            ssh_port_mapping = json.loads(Path("/home/config/ssh/port-mapping.json").read_bytes())
        except FileNotFoundError:
            ssh_port_mapping = None

        env = {
            "cluster": {
                "chief": ["main1:" + get_ssh_port(ssh_port_mapping, "main1")],
                "worker": [],
            },
        }
        for cluster_host in os.environ["BACKENDAI_CLUSTER_HOSTS"].split(","):
            if cluster_host != "main1":
                env["cluster"]["worker"].append(
                    cluster_host + ":" + get_ssh_port(ssh_port_mapping, cluster_host)
                )

        # TF's worker index starts from 0.
        cluster_role_idx = str(int(os.environ["BACKENDAI_CLUSTER_IDX"]) - 1)
        if os.environ["BACKENDAI_CLUSTER_ROLE"] == "main":
            task = {
                "type": "chief",
                "index": cluster_role_idx,
            }
        else:
            task = {
                "type": "worker",
                "index": cluster_role_idx,
            }
        env["task"] = task
        print(json.dumps(env))
    else:
        print("")

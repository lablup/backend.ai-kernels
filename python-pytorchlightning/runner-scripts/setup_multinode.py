import os
import json

if 'BACKENDAI_CLUSTER_HOST' in os.environ: # Start mutli-instance setup.
    env = {}
    env['cluster'] = {}
    env['cluster']['worker'] = []
    for container in os.environ['BACKENDAI_CLUSTER_HOSTS'].split(","):
        env['cluster']['worker'].append(container + ":2220")
    env['task'] = {}
    if os.environ['BACKENDAI_CLUSTER_ROLE'] == 'main':
        env['task']['type'] = "worker" # Was chief. but recent TF choose first worker as chief.
        env['task']["index"] = str(int(os.environ['BACKENDAI_CLUSTER_IDX']) - 1) # Index starts from 0
    else:
        env['task']['type'] = "worker"
        env['task']["index"] = os.environ['BACKENDAI_CLUSTER_IDX'] 
    print(json.dumps(env))
else:
    print("")

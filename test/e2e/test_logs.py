import subprocess
print("--- dog-trainer logs ---")
print(subprocess.run(['docker-compose', '-p', 'dog_e2e', '-f', '../../../docker-compose.local_deploy.yml', 'logs', '--tail', '100', 'dog-trainer'], capture_output=True, text=True).stdout)

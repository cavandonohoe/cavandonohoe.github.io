#!/usr/bin/env python3
import json, os, subprocess
from datetime import datetime, timezone, timedelta

repo=os.environ.get('GITHUB_REPOSITORY','cavandonohoe/cavandonohoe.github.io')
since=datetime.now(timezone.utc)-timedelta(days=30)
prs=json.loads(subprocess.check_output(['gh','api',f'/repos/{repo}/pulls?state=closed&per_page=100'],text=True))
events=[]
for p in prs:
    if not p.get('merged_at'): continue
    dt=datetime.fromisoformat(p['merged_at'].replace('Z','+00:00'))
    if dt < since: continue
    events.append({'date':dt.date().isoformat(),'type':'merged_pr','title':p['title'],'url':p['html_url']})
events.sort(key=lambda x:x['date'], reverse=True)
os.makedirs('data',exist_ok=True)
open('data/changelog.json','w').write(json.dumps({'generated_at':datetime.now(timezone.utc).isoformat(),'events':events},indent=2)+'\n')

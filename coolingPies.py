t = int(input())
for i in range(t):
	number_of_pies=int(input())
	wt=list(map(int,input().split()))
	wtlim=list(map(int,input().split()))
	wt=sorted(wt)
	wtlim=sorted(wtlim)
	rack_status=[0]*number_of_pies
	for k in range(number_of_pies-1,-1,-1):
		temp = -1
		for l in range(number_of_pies-1,-1,-1):
			if ( ( wt[k] <= wtlim[l] ) and (rack_status[l]!=1)):
				temp=l
		if(temp!=-1):
			rack_status[temp]=1	
	total=sum(rack_status)
	print(total)


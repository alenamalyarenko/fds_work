cd ('/mnt/data2/data2/FDS/PALM_outputs/')
%%
filename='DATA_3D_NETCDF_N03';
%%
clear u_in v_in w_in temp_in time
%ncread goes from 0, so everything is -1
u_in=ncread(filename,'u',[156 156 2 1],[62 62 22 60]);
v_in=ncread(filename,'v',[156 156 2 1],[62 62 22 60]);
w_in=ncread(filename,'w',[156 156 2 1],[62 62 22 60]);

temp_in=ncread(filename,'theta',[156 156 2 1],[62 62 22 60]);

time=ncread(filename,'time',[1],[60]);
%%

%%
for i=1:6000
%0.01s
 time_1s(i,1)=time(1)+i*0.01;
end

%%

%%
clear u_rec
for i=1:62
    for j=1:62
        for k=1:22
u_rec(i,j,k,:)=interp1(time(:,1),squeeze(u_in(i,j,k,:)),time_1s);
v_rec(i,j,k,:)=interp1(time(:,1),squeeze(v_in(i,j,k,:)),time_1s);
w_rec(i,j,k,:)=interp1(time(:,1),squeeze(w_in(i,j,k,:)),time_1s);
temp_rec(i,j,k,:)=interp1(time(:,1),squeeze(temp_in(i,j,k,:)),time_1s);
        end
    end
end

%%
nt=6000;
ibp2=62; jbp2=62; kbp2=22;
ibp1=61; jbp1=61; kbp1=21;

for t=1:nt
   %!SOUTH  
   for i=1:ibp2
      for k=1:kbp2       
          VS(i,k,t)=v_rec(i,1,k,t);
          
          WS1(i,k,t)=w_rec(i,1,k,t);
          WS2(i,k,t)=w_rec(i,2,k,t);      
      end
   end
   

   
   %!NORTH  
   for i=1:ibp2
      for k=1:kbp2       
          VN(i,k,t)=v_rec(i,jbp1,k,t);
          
          WN1(i,k,t)=w_rec(i,jbp1,k,t);
          WN2(i,k,t)=w_rec(i,jbp2,k,t);         
      end
   end

   
   
   %!EAST
   for j=1:jbp2
      for k=1:kbp2       
          UE(j,k,t)=u_rec(ibp1,j,k,t);
         
          WE1(j,k,t)=w_rec(ibp1,j,k,t);
          WE2(j,k,t)=w_rec(ibp2,j,k,t);             
      end
   end

   
   %!WEST
   for j=1:jbp2
      for k=1:kbp2       
          UW(j,k,t)=u_rec(1,j,k,t);
          
          WW1(j,k,t)=w_rec(1,j,k,t);
          WW2(j,k,t)=w_rec(2,j,k,t);
      end
   end


end

%%

filename_out='pal9.nc';
nccreate(filename_out,'UE',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'UE',UE)

nccreate(filename_out,'UW',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'UW',UW)

nccreate(filename_out,'WE1',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WE1',WE1)

nccreate(filename_out,'WW1',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WW1',WW1)

nccreate(filename_out,'WE2',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WE2',WE2)

nccreate(filename_out,'WW2',"Dimensions",{"y",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WW2',WW2)



nccreate(filename_out,'VS',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'VS',VS)

nccreate(filename_out,'VN',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'VN',VN)

nccreate(filename_out,'WS1',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WS1',WS1)

nccreate(filename_out,'WN1',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WN1',WN1)

nccreate(filename_out,'WS2',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WS2',WS2)

nccreate(filename_out,'WN2',"Dimensions",{"x",62,"z",22,"time",6000},"FillValue","disable")
ncwrite(filename_out,'WN2',WN2)


%%


% filename_out='palm_output_high_freq.nc';
% nccreate(filename_out,'u',"Dimensions",{"x",62,"y",62,"z",22,"time",6000},"FillValue","disable")
% ncwrite(filename_out,'u',u_rec)
% 
% nccreate(filename_out,'v',"Dimensions",{"x",62,"y",62,"z",22,"time",6000},"FillValue","disable")
% ncwrite(filename_out,'v',v_rec)
% 
% nccreate(filename_out,'w',"Dimensions",{"x",62,"y",62,"z",22,"time",6000},"FillValue","disable")
% ncwrite(filename_out,'w',w_rec)
% 
% nccreate(filename_out,'theta',"Dimensions",{"x",62,"y",62,"z",22,"time",6000},"FillValue","disable")
% ncwrite(filename_out,'theta',u_rec)





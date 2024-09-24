% name='Coupled6_4'; meshes=25; 
 %gris cells per mesh:
% xmesh=10; ymesh=10; zmesh=120;  
% I_upper=5;J_upper=5;
 %size of mesh  in meters
% x_m=20; y_m=20;z_m=120;
 


 name='Palm1_1'; meshes=64; 
 %gris cells per mesh: IJK
 xmesh=32; ymesh=32; zmesh=256;
 I_upper=8;J_upper=8;
 %size of mesh  in meters: DX DY DZ 
 x_m=128; y_m=128;z_m=1024;



startpoint=pwd;



path='/nesi/project/vuw03158/fds/palm2';
name_to_save=['matlab_',name ,'.mat'];

last_time_step=14400;




[xc,yc,zc,xu,yu,zu,xv,yv,zv,xw,yw,zw]=set_up_coordinates(xmesh*I_upper,ymesh*J_upper,zmesh,x_m/xmesh,y_m/ymesh,z_m/zmesh);

% load the run
cd(path)

count=0;
for time=1:last_time_step
  second=time-1;
    skipit=false;
    for par=1:100
        if (par-1)<10
            part=['0' num2str(par-1)];
        else
            part=num2str(par-1);
        end
        filename=[name,'_1_',num2str(second), 'p',num2str(part),'.q.nc'];
        check=isfile(filename);
        if (check==1)  %file exist, read variables 
            count=count+1;
            filelist(count,:)={filename};
            time_stamp(count)=time;
            skipit=true;

 
            for nm=1:meshes
                clear TMP_v TMP_u TMP_t TMP_w

                 filename2=[name,'_',num2str(nm),'_',num2str(second), 'p',num2str(part),'.q.nc'];
                 % disp(filename2)

                ncid = netcdf.open(filename2, 'NC_NOWRITE');
                TMP_v = netcdf.inqVarID(ncid, 'v');
                TMPV(:,:,:,nm) = netcdf.getVar(ncid, TMP_v);
                TMP_u = netcdf.inqVarID(ncid, 'u');
                TMPU(:,:,:,nm) = netcdf.getVar(ncid, TMP_u);
                TMP_w = netcdf.inqVarID(ncid, 'w');
                TMPW(:,:,:,nm) = netcdf.getVar(ncid, TMP_w);
                TMP_t = netcdf.inqVarID(ncid, 'temp');
                TMPT(:,:,:,nm) = netcdf.getVar(ncid, TMP_t);
                netcdf.close(ncid);
            end
       

            % switch meshes
            % 
            %     case 1
            %         U(:,:,:,count)=TMPU(:,:,:);
            %         V(:,:,:,count)=TMPV(:,:,:);
            %         W(:,:,:,count)=TMPW(:,:,:);
            %         T(:,:,:,count)=TMPT(:,:,:);          	              
            % 
            %     otherwise

                    %temp
                    n_mesh=0;
                    clear dum2
                    for j=1:J_upper
                        clear dum
                        for i=1:I_upper
                            n_mesh=n_mesh+1; %global mesh counter
                            %glue in i:
                            if (i==1)
                                dum(:,:,:,:)=TMPT(:,:,:,n_mesh);
                            else
                                dum=cat(1,dum,TMPT(:,:,:,n_mesh));
                            end
                        end   
                        %now glue in j
                        if (j==1)                    
                            dum2(:,:,:)=dum;
                        else
                            dum2=cat(2,dum2,dum);
                        end
                    end %j meshes
                    T(:,:,:,count)=dum2;


                    %W
                    n_mesh=0;
                    clear dum2
                    for j=1:J_upper
                        clear dum
                        for i=1:I_upper
                            n_mesh=n_mesh+1; %global mesh counter
                            %glue in i:
                            if (i==1)
                                dum(:,:,:,:)=TMPW(:,:,:,n_mesh);
                            else
                                dum=cat(1,dum,TMPW(:,:,:,n_mesh));
                            end
                        end   
                        %now glue in j
                        if (j==1)                    
                            dum2(:,:,:)=dum;
                        else
                            dum2=cat(2,dum2,dum);
                        end
                    end %j meshes
                    W(:,:,:,count)=dum2;


                    % U
                    n_mesh=0;
                    clear dum2
                    for j=1:J_upper
                        clear dum
                        for i=1:I_upper
                            n_mesh=n_mesh+1; %global mesh counter
                            %glue in i:
                            if (i==1)
                                dum(:,:,:,:)=TMPU(1:(end-1),:,:,n_mesh);
                            elseif (i==I_upper) %most right mesh, take the last value
                                dum=cat(1,dum,TMPU(1:end,:,:,n_mesh));
                            else
                                dum=cat(1,dum,TMPU(1:(end-1),:,:,n_mesh));
                            end
                        end   
                        %now glue in j
                        if (j==1)                    
                            dum2(:,:,:)=dum;
                        else
                            dum2=cat(2,dum2,dum);
                        end
                     end %j meshes
                     U(:,:,:,count)=dum2;
                    
                    % V
                    n_mesh=0;
                    clear dum2
                    for j=1:J_upper
                        clear dum
                        for i=1:I_upper
                            n_mesh=n_mesh+1; %global mesh counter
                            
                            %special condition for top mesh, take last V value
                            if (j==J_upper)
                              %glue in i:
                                if (i==1)
                                    dum(:,:,:,:)=TMPV(:,1:(end),:,n_mesh);
                                else
                                    dum=cat(1,dum,TMPV(:,1:(end),:,n_mesh));
                                end
                            else
                                %glue in i:
                                if (i==1)
                                    dum(:,:,:,:)=TMPV(:,1:(end-1),:,n_mesh);
                                else
                                    dum=cat(1,dum,TMPV(:,1:(end-1),:,n_mesh));
                                end
                            end
                        end   
                        %now glue in j
                        if (j==1)                    
                            dum2(:,:,:)=dum;
                        else
                            dum2=cat(2,dum2,dum);
                        end
                     end %j meshes
                     V(:,:,:,count)=dum2;

                

                
            % end %mashes case
         end %file exist, read variables  
         if skipit
            break; %break out of partial time loop if file is found
         end
     end %partial time loop
end %time loop  




cd (startpoint);
save(name_to_save,'-v7.3')
%%

function [xc,yc,zc,xu,yu,zu,xv,yv,zv,xw,yw,zw]=set_up_coordinates(xmesh,ymesh,zmesh,dx,dy,dz)


for i=1:xmesh
    for j=1:ymesh
        for k=1:zmesh
           xc(i,j,k)=dx/2+dx*(i-1);
           yc(i,j,k)=dy/2+dy*(j-1);
           zc(i,j,k)=dz/2+dz*(k-1);
        end
    end
end

for i=1:(xmesh+1)
    for j=1:ymesh
        for k=1:zmesh
           xu(i,j,k)=dx*(i-1);
           yu(i,j,k)=dy/2+dy*(j-1);
           zu(i,j,k)=dz/2+dz*(k-1);      
           
        end
    end
end

for i=1:xmesh
    for j=1:(ymesh+1)
        for k=1:zmesh
           xv(i,j,k)=dx/2+dx*(i-1);
           yv(i,j,k)=dy*(j-1);
           zv(i,j,k)=dz/2+dz*(k-1);
        end
    end
end

for i=1:xmesh
    for j=1:ymesh
        for k=1:(zmesh+1)
           xw(i,j,k)=dx/2+dx*(i-1);
           yw(i,j,k)=dy/2+dy*(j-1);
           zw(i,j,k)=dz*(k-1);
        end
    end
end



end %end function
program password
implicit none
character(1) :: lma(26)=(/"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"/)
character(1) :: lmi(26)=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"/)
character(1) :: num(10)=(/"0","1","2","3","4","5","6","7","8","9"/)
character(1) :: ascii(25)=(/'!','"','#','$','%','&',"'",'(',')','*','+',':',';','<','=','>','?','@','[',']','^',',','_','{','}'/)
character(1) :: ascii2(29)
character(1),dimension (:), allocatable :: v1
character(1),dimension (:), allocatable :: v2
character(1) :: rma,rmi,rnum,rmasc
integer :: tamanho
character(3) :: nome
integer :: i=1,t,t1=0,t2=0,t3=0,t4=0,c,v_random, z=8,p,tempo
real :: r=0
integer, dimension (8) :: semente


do p=1,z
call system_clock(count=tempo)
semente(p)=tempo 
end do
call random_seed(size=z)
call random_seed(put=semente)

!Arranjo ascii2 foi criado porque por algum motivo não consegui inserir 4
!caracteres no momento da declaração de ascii
ascii2(1:25)=ascii
ascii2(26)='\'
ascii2(27)='~'
ascii2(28)='`'
ascii2(29)='|'


print *, "A senha deve conter letras maiúsculas? Responda S ou N."
read(*,*)rma


print *, "A senha deve conter letras minúsculas? Responda S ou N."
read(*,*)rmi


print *, "A senha deve conter números? Responda S ou N."
read(*,*)rnum

print *, "A senha deve conter caracteres especiais? Responda S ou N."
read(*,*)rmasc


print *, "A senha deve conter quantos caracteres?"
read(*,*)c

if (rma=="S") then
t1=26
end if
if (rmi=="S") then
t2=26
end if
if (rnum=="S") then
t3=10
end if
if (rmasc=="S") then
t4=29
end if


!Vetor que recebe o password
allocate(v2(c))
t=t1+t2+t3+t4
allocate(v1(t))

if (t==29) then
v1(1:29)=ascii2
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==39) then
v1(1:29)=ascii2
v1(30:39)=num
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==55) then
if (rma=="S") then
v1(1:26)=lma
else
v1(1:26)=lmi
end if
v1(27:55)=ascii2
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==65) then
deallocate(v1)
allocate(v1(t+10))
v1(1:29)=ascii2
v1(30:39)=num
if (rma=="S") then
v1(40:65)=lma
else
v1(40:65)=lmi
end if
v1(66:75)=num
do while (i<=c)
call random_number(r)
v_random=floor((t+10+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==81) then
v1(1:26)=lma
v1(27:52)=lmi
v1(53:81)=ascii2
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if
  
  
if (t==91) then
deallocate(v1)
allocate(v1(t+10))
v1(1:26)=lma
v1(27:52)=lmi
v1(53:62)=num
v1(63:91)=ascii2
v1(92:101)=num
do while (i<=c)
call random_number(r)
v_random=floor((t+10+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if





if (t==26) then
if (rma=="S") then
v1(1:26)=lma
else
v1(1:26)=lmi
end if
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if




if (t==52) then
v1(1:26)=lma
v1(27:52)=lmi
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==36) then
if (rma=="S") then
v1(1:26)=lma
else
v1(1:26)=lmi
end if
v1(27:36)=num
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

if (t==62) then
v1(1:26)=lma
v1(27:52)=lmi
v1(53:62)=num
do while (i<=c)
call random_number(r)
v_random=floor((t+1)*r)
if(v1(v_random)/=v2(i-1)) then
v2(i)=v1(v_random)
i=i+1
end if
end do
end if

print *, v2


end program password


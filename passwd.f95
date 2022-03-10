program password
use words
implicit none
!Para não haver repetição, a geração da semente aleatória depende
!da hora
	call system_clock(count=tempo)
	   do p=1,z
	      semente(p)=tempo 
           end do
        call random_seed(size=z)
        call random_seed(put=semente)
!Arranjo ascii2 foi criado porque, por algum motivo, não consegui inserir 4
!caracteres no momento da declaração de ascii
	ascii2(1:25)=ascii
	ascii2(26)='\'
	ascii2(27)='~'
	ascii2(28)='`'
	ascii2(29)='|'
	print *, "A senha deve conter quantos caracteres?"
	read(*,*)c
	print *, "A senha deve conter letras maiúsculas? Responda s ou n."
	read(*,*)rma
	print *, "A senha deve conter letras minúsculas? Responda s ou n."
	read(*,*)rmi
	print *, "A senha deve conter números? Responda s ou n."
	read(*,*)rnum
	print *, "A senha deve conter caracteres especiais? Responda s ou n."
	read(*,*)rmasc
	   if (rma=="S" .or. rma=="s") then
		t1=26
	   end if
	   if (rmi=="S" .or. rmi=="s") then
		t2=26
	   end if
	   if (rnum=="S" .or. rnum=="s") then
		t3=10
	   end if
	   if (rmasc=="S" .or. rmasc=="s") then
		t4=29
	   end if
!Vetor que recebe o password
	allocate(v2(c))
!Alocação do vetor que receberá os vetores correspondentes a seleção
!do usuário
	t=t1+t2+t3+t4
	allocate(v1(t))
!password com apenas letras maiúsculas ou apenas letras minúsculas
	   if (t==26) then
		if (rma=="S" .or. rma=="s") then
		   v1(1:26)=lma
	        else
		   v1(1:26)=lmi
		end if
		call passwd1 ()
	   end if
!password apenas com números
	   if (t==10) then
		v1(1:10)=num
		call passwd1 ()
	   end if
!password apenas com caracteres especiais
	   if (t==29) then
		v1(1:29)=ascii2
		call passwd1 ()
	   end if
!password com letras maiúsculas e minúsculas
	   if (t==52) then
		v1(1:26)=lma
		v1(27:52)=lmi
		allocate(b(c))
		   a1=27
		call passwd2 ()
	   end if
!password com números e letras maiúsculas ou minúsculas 
!(apenas uma opção)
	   if (t==36) then
		if (rma=="S" .or. rma=="s") then
		   v1(1:26)=lma
		else
	 	   v1(1:26)=lmi
	   	end if
		   v1(27:36)=num
		   allocate(b(c))
		   a1=27
		   call passwd2 ()
	   end if
!password com letras maíusculas ou minúsculas e caracteres especiais
	   if (t==55) then
		if (rma=="S" .or. rma=="s") then
		  v1(1:26)=lma
		else
		  v1(1:26)=lmi
		end if
		v1(27:55)=ascii2
		allocate(b(c))
		a1=27
		call passwd2 ()
	   end if
!Password com caracteres especiais e números
	   if (t==39) then
		v1(1:29)=ascii2
		v1(30:39)=num
		allocate(b(c))
		a1=30
		call passwd2 ()
	   end if
!password com números, letras maiúsculas ou minúsculas (apenas
!uma alternativa) e caracteres especiais.
	   if (t==65) then
		v1(1:29)=ascii2
		v1(30:39)=num
		  if (rma=="S" .or. rma=="s") then
		    v1(40:65)=lma
		  else
		    v1(40:65)=lmi
		  end if
		allocate(b(c))
		a1=30
		a2=40
		call passwd3 ()
	   end if
!passoword com letras maiúsculas, minúsculas e caracteres especiais
	   if (t==81) then
	       v1(1:26)=lma
	       v1(27:52)=lmi
	       v1(53:81)=ascii2
	       allocate(b(c))
		a1=27
		a2=53
		call passwd3 ()
	   end if
!password com letras maiúsculas, letras minúsculas e números.
	   if (t==62) then
		v1(1:26)=lma
		v1(27:52)=lmi
		v1(53:62)=num
		allocate(b(c))
		a1=27
		a2=53
		call passwd3 ()
	   end if
!password com todas as opções disponíveis.
	   if (t==91) then
		v1(1:26)=lma
		v1(27:52)=lmi
		v1(53:62)=num
		v1(63:91)=ascii2
		allocate(b(c))
		a1=27
		a2=53
		a3=63
		call passwd4 ()	   
	   end if
print *, 'A senha a seguir atende aos requisitos: ',v2
deallocate (v2)
end program password


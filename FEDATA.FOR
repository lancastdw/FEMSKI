      dimension ipops(20), itype(20), lnods(8),
     .	 lnomin(12), lnomax(12), lselec(9,12), maxdim(12), nasty(10),
     .	 ndftab(12), nzmin(20), nzmax(20), xyz(6)
      character blank*11, line(2)*80, lprops(2,21)*11, minus*11,
     .	 props(2,21)*11, quest(21)*50, sixate*11, sixnin*11,
     .	 type(12)*58, vprop(21)*11, zero*11,junk*80
      common/text/type, quest, props
c
c*** program to assemble the data for femski at an interactive terminal.
c
      data ipops/20*0/, iterat/0/, itype/20*0/,
     .	 lnomax/8,8,8,8,8,8,2,4,8,3,5,8/,
     .	 lnomin/3,3,3,3,3,3,2,4,8,3,5,6/,
     .	 maxdim/2,2,2,2,2,2,3,2,3,2,3,3/, maxrhs/1/, nasty/10*0/,
     .	 ndftab/1,2,1,2,3,1,6,2,3,3,5,5/, newrhs/1/, nzmax/20*0/,
     .	 nzmin/20*99/, xyz/6*0.0/
      data lselec/
     1	 4,13,14,15,16,12,17,0,0,
     2	 1,2,10,4,17,18,19,0,0,
     3	 1,2,17,0,0,0,0,0,0,
     4	 1,2,9,17,18,19,20,0,0,
     5	 1,2,10,17,0,0,0,0,0,
     6	 1,2,17,0,0,0,0,0,0,
     7	 1,3,5,6,7,8,11,18,19,
     8	 1,2,10,4,18,19,0,0,0,
     9	 1,2,11,18,19,0,0,0,0,
     .	 1,2,11,4,12,18,21,0,0,
     1	 1,2,11,4,12,18,19,0,0,
     2	 1,3,5,6,7,8,11,18,19/
      data minus/'-          '/, sixnin/'999999   '/,
     .	zero/'0.0            '/, blank/'               '/,
     . sixate/'888888      '/
      open(5, file = 'file05',form = 'formatted', status='new')
      open(1, file = 'file01',form = 'formatted', status='new')
      open(2, file='file02', form = 'formatted', status='new')
      open(3, file='file03', form = 'formatted', status='new')
      open(4, file='file04', form = 'formatted', status='new')
      open(7, file='file07', form = 'formatted', status='new')
      open(8, file='file08', form = 'formatted', status='new')
      open(9, file='file09', form = 'formatted', status='new')
      write(*,*)'First enter a one line title for this problem'
      read(*,107)junk
      write(5,107)junk
      write(*,102)
 102  format(//' At any point you can redo the previous by entering 8888
     .88 and hitting RETURN')
    4 write(*,104)
 104  format(//' Now to work -  Lets get started. First I want you to t
     .ype the',/' PROPERTY-LIST number for each element, followed by its
     .',/' NODE-NUMBERS in anticlockwise order: one line per element',/
     .'Use free format throughout: After the last element "break" the fo
     .rmat by		an invalid entry eg. QUIT')
c
c*** write the element topology cards, giving all the nodal connections.
c
      rewind 1
      rewind 2
      nelz = 0
      jprop = 0
      line(2) = '0 0 0 0 0 0 0 0'
    6 nel = nelz + 1
      write(*,106) nel
 106  format(8h Element,i2)
      read(*,107) line(1)
 107  format(a80)
      write(1,107) line
      rewind 1
      read(1,*, err = 11) lpo, (lnods(n), n = 1,8)
      rewind 1
      if(lpo.eq.999999) go to 4
      if(lpo.ne.888888) go to 8
      backspace 2
      read(2,211) nelz, lpop, (lnods(n), n = 1,8)
      backspace 2
      write(*,110)
 110  format(/' You typed what follows - you can re-type it now.')
      write(*,211) nelz, lpop, (lnods(n), n = 1,8)
      nelz = nelz - 1
      if(nelz.eq.-1) nelz = 0
      go to 6
    8 lpop = iabs(lpo)
      if(lpop.eq.0) lpop = 1
      ipops(lpop) = 1
      if(lpop.gt.jprop) jprop = lpop
      nz = 0
      do 10 n = 1,8
      nod = lnods(n)
      if(nod.ne.0) nz = n
   10 continue
      if(nz.gt.nzmax(lpop)) nzmax(lpop) = nz
      if(nz.lt.nzmin(lpop)) nzmin(lpop) = nz
      nelz = nelz + 1
      if(nelz.eq.1) write(2,210)
 210  format(7hElement,4x,9hProp.list,5x,12hNode numbers)
      write(2,211) nelz, lpop, (lnods(n), n = 1,8)
 211  format(i4,2i11,7i4)
      if(lpo.ge.0) go to 6
c
c*** input the element property-lists following cues from the computer.
c
   11 rewind 1
      rewind 3
      lprop = 0
      ndfmax = 0
   12 lprop = lprop + 1
      if(ipops(lprop).eq.0) nasty(1) = 1
   14 write(*,114) lprop
 114  format(/' Element property-list number',i3,' what TYPE NUMBER had
     .you in mind?')
      read(*,*, err = 24) lty
      if(lty.eq.999999) go to 6
      ltyp = iabs(lty)
      if(nzmin(lprop).lt.lnomin(ltyp)) nasty(2) = 1
      if(nzmax(lprop).gt.lnomax(ltyp)) nasty(3) = 1
      write(*,116) type(ltyp)
 116  format(a58//'0123456789........Do not trespass outside this 10 cha
     .racter field.')
      if(ndftab(ltyp).gt.ndfmax) ndfmax = ndftab(ltyp)
      itype(lprop) = ltyp
      do 16 np = 1,21
      vprop(np) = zero
      do 16 j = 1,2
   16 lprops(j,np) = blank
      np = 0
   18 np = np + 1
      if(np.gt.9) go to 22
      nm = lselec(np,ltyp)
      if(nm.eq.0) go to 22
      write(*,118) quest(nm)
 118  format(a48)
      do 20 j = 1,2
   20 lprops(j,np) = props(j,nm)
      read(*,120) vprop(np)
 120  format(a11)
      if(vprop(np).eq.sixnin) go to 14
      if(vprop(np).eq.minus) go to 22
      if(vprop(np).ne.sixate) go to 18
      if(np.ge.2) np = np - 2
      go to 18
   22 np = np - 1
      write(3,322) lprop, ltyp, type(ltyp),
     .	 (((lprops(j,n7-1+n), n = 1,7), j = 1,2),
     .	 (vprop(n7-1+n),	n = 1,7),	n7 = 1,np,7)
 322  format(13hProperty list,i2,4x,30h(Integer here and in next line,
     .	 28h must be in correct column.)/13h Element type,i2,4x,a58/
     .	 (2x,7a11))
      if(lty.lt.0) go to 24
      if(lprop.lt.jprop) go to 12
c
c*** coordinate cards include a code saying which variables are earthed.
c
   24 rewind 4
      ncord = 0
      nafix = 0
      ndim = 0
      write(*,123)
 123  format(/49h Use 888888 as node number to redo previous line.)
   25 write(0,125)
 125  format(/52h Now write some of the node numbers, followed by the/
     .	 59h fixing code (zero if the node is free) and by x,y or x,y,z/
     .	 56hNote, you must present the last node-number as negative./
     .	 4hnode,4x,6hfixing,2x,11hcoordinates/6hnumber,2x,4hcode,3x,
     .	 1hx,5x,1hy,5x,1hz)
      do 26 np = 1,20
      if(itype(np).ne.0) nd = maxdim(itype(np))
      if(ndim.ne.0. and .ndim.ne.nd) nasty(4) = 1
   26 ndim = nd
   28 read(*,*, err = 30) no, kod, (xyz(n), n = 1,ndim)
      if(no.eq.999999) go to 12
      if(no.ne.888888) go to 29
      backspace 4
      read(4,430) no, kod, (xyz(n), n = 1,ndim)
      backspace 4
      write(*,110)
      write(*,430) no, kod, (xyz(n), n = 1,ndim)
      ncord = ncord - 1
      if(ncord.lt.0) ncord = 0
      if(kod.ne.0) nafix = nafix - 1
      go to 28
   29 ncord = ncord + 1
      if(ncord.eq.1) write(4,429)
 429  format('Nodes    Fixing-codes   Nodal coordinates')
      write(4,430) no, kod, (xyz(n), n = 1,ndim)
 430  format(i3,i12,3(f18.7,1x))
      maxkod = 10**ndfmax/9
      if(kod.gt.maxkod) nasty(5) = 1
      if(kod.ne.0) nafix = nafix + 1
      if(no.gt.0) go to 28
c
c*** some nodes have prescribed nonzero values - need special treatment.
c
   30 rewind 7
      nbfix = 0
   31 write(*,131)
 131  format(/47hIs that all the FIXITIES? Are there any NONZERO,
     .	 19h prescribed values?/25hIf so, then type the node,
     .	 41h number, the fixing code, and the values,/9hincluding,
     .	 35h zeros for those that aren't fixed./12hTell me when,
     .	 46h you've finished, with a negative node-number.,
     ./'If there are none enter "no"')
   32 read(*,*, err = 34) no, kod, (xyz(nd), nd = 1,ndfmax)
      if(no.eq.999999) go to 25
      if(no.ne.888888) go to 33
      backspace 7
      read(7,734) nod, kod, (xyz(nd), nd = 1,ndfmax)
      backspace 7
      write(*,110)
      write(*,734) nod, kod, (xyz(nd), nd = 1,ndfmax)
      nbfix = nbfix - 1
      go to 32
   33 nbfix = nbfix + 1
      if(nbfix.eq.1) write(7,733)
 733  format(44hNode  Fixing-code  Nonzero prescribed values)
      nod = iabs(no)
      write(7,734) nod, kod, (xyz(nd), nd = 1,ndfmax)
 734  format(i3,i9,2x,6(f11.6,1x))
      if(kod.gt.maxkod) nasty(6) = 1
      if(no.gt.0) go to 32
c
c*** we can apply loads to certain specified nodes by hand if we so wish
c
   34 nfix = nafix + nbfix
      rewind 8
      nload = 0
   35 write(*,135)
 135  format(/50hHave you any EXTRA LOADS, that I don't know about?/
     .	 53hGive me the node-numbers, followed by the nodal loads,
     .	 9h in turn./32hEnd with a negative node-number.,
     .	 /'If there are none enter "no"')
   36 read(*,*, err = 38) no, (xyz(nv), nv = 1,ndfmax)
      if(no.eq.999999) go to 31
      if(no.ne.888888) go to 37
      backspace 8
      read(8,837) nod, (xyz(nv), nv = 1,ndfmax)
      backspace 8
      write(*,110)
      write(*,837) nod, (xyz(nv), nv = 1,ndfmax)
      nload = nload - 1
      go to 36
   37 nload = nload + 1
      if(nload.eq.1) write(8,836)
 836  format(4hNode,5x,16hAdditional loads)
      nod = iabs(no)
      write(8,837) nod, (xyz(nv), nv = 1,ndfmax)
 837  format(i3,4x,6(f14.6,1x))
      if(no.gt.0) go to 36
c
c*** the option exists to introduce certain additional springs to earth.
c
   38 rewind 9
      nextif = 0
      write(*,138)
 138  format(/45hFinally, I can accept extra SPRINGS TO EARTH./
     .	 55hGive me their stiffnesses, preceded by the node number./
     .	 39hSignal the last node with a minus sign.)
   40 read(*,*, err = 44) no, (xyz(nv), nv = 1,ndfmax)
      if(no.eq.999999) go to 35
      if(no.ne.888888) go to 42
      backspace 9
      read(9,837) nod, (xyz(nv), nv = 1,ndfmax)
      backspace 9
      write(*,110)
      write(*,837) nod, (xyz(nv), nv = 1,ndfmax)
      nextif = nextif - 1
      go to 40
   42 nextif = nextif + 1
      if(nextif.eq.1) write(9,942)
 942  format(4hNode,5x,17hExtra stiffnesses)
      nod = iabs(no)
      write(9,837) nod, (xyz(nv), nv = 1,ndfmax)
      if(no.gt.0) go to 40
c
c*** make certain that the final coordinate card has a negative node number.
c
   44 backspace 4
      read(4,430) no, kod, (xyz(n), n = 1,ndim)
      backspace 4
      no = -iabs(no)
      write(4,430) no, kod, (xyz(n), n = 1,ndim)
c
c*** insert special cards for those sections with no data at all.
c
      if(nbfix.eq.0) write(7,746)
 746  format(42hThere are no nonzero fixities in this job.)
      if(nload.eq.0) write(8,847)
 847  format(55hThere are no extra loads, apart from gravity loads etc.)
      if(nextif.eq.0) write(9,948)
 948  format(41hThere are no additional springs to earth.)
c
c*** dimension card, the first, is actually written the last of all.
c
      rewind 1
      write(1,1049) nelz, nfix, nload, nextif, newrhs, maxrhs, iterat
1049  format(8hHow many,3x,8hHow many,3x,9hNo.loaded,2x,9hNo.sprung,
     .	 2x,5hNo.of,6x,7hMaximum,4x,9hiterative/9helements?,2x,
     .	 10hfix.nodes?,1x,6hnodes?,5x,6hnodes?,5x,10hR.H.sides?,1x,
     .	 10hR.H.sides?,1x,10hsolutions?/i4,6i11)
c
c*** exit, but first we must present the errors at the terminal display.
c
      nast = 0
      do 50 n = 1,10
      if(nasty(n).eq.0) go to 50
      nast = 1
      write(*,150) n
 150  format(//6hError,i3,9h in input)
   50 continue
      if(nast.ne.0) write(*,151)
 151  format(/40h You CAN go back and input it all again!/
     .	 7x,22hOr you can edit file05)
      close(1)
      close(2)
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      stop
      end
c
c
      block data
c
c*** to set the prompting comments for the f.e. input program, "fedata".
c
      common/text/type, quest, props
      character type(12)*58, quest(21)*50, questa(10)*50, questb(11)*50,
     .	 props(2,21)*11
      equivalence (questa(1),quest(1)), (questb(1),quest(11))
      data type/
     1	 '- Thats the flat stretched membrane sitting on springs?   ',
     2	 '- Thats the isoparametric plane stress:strain element?    ',
     3	 '- You want to twist something you can turn on a lathe?    ',
     4	 '- An axisymmetric object, with pressure, endload, etc?    ',
     5	 '- You want to bend something you could turn on a lathe?   ',
     6	 '- You want to find shear and torsion, in a uniform beam?  ',
     7	 '- You want to use the simple beam, in three dimensions?   ',
     8	 '- Thats the super-duper hybrid plane stress:strain quad?  ',
     9	 '- Not the incredible 8-node hybrid brick?  Good luck ...  ',
     .	 '- The high-performance plate bending triangle and quad?   ',
     1	 '- The Semiloof shell? - Thats a terribly complicated one. ',
     2	 '- The Semiloof beam?  Do remember the two extra nodes.    '/
      data questa/
     1	 '  What is Youngs modulus, E? (for direct tension.)',
     2	 '  What is Poissons ratio, v?                     ',
     3	 '  What is the Shear modulus, G?(for shear stress.)',
     4	 '  What is the thickness?  (Zero for plane strain.)',
     5	 '  What is the cross-sectional area of the beam?   ',
     6	 '  What is Izz, usually against vertical bending?  ',
     7	 '  What is Ixx, perpendicular to Izz?              ',
     8	 '  What is the polar moment of inertia, J?         ',
     9	 '  The density of the material, for gravity loads? ',
     .	 '  The density of the material, for gravity loads? '/
      data questb/
     1	 '  The density of the material, for gravity loads? ',
     2	 '  The pressure acting normal to the paper usually?',
     3	 '  The stiffness to earth, per unit membrane area? ',
     4	 '  The xx-stress, in the plane of the membrane?    ',
     5	 '  The yy-stress which should stiffen the membrane?',
     6	 '  The xy-shear stress in plane of the membrane?   ',
     7	 '  Use an NxN Gauss rule: what is N (with decimal?)',
     8	 '  The coefficient of linear thermal expansion?    ',
     9	 '  What is the temperature of this element type?   ',
     .	 '  The angular velocity in rads/sec. about x-axis? ',
     .	 '  Temperature Difference, between top and bottom? '/
      data props/
     1	 'Youngs     ','Modulus,E  ',    'Poissons  ','ratio,v    ',
     3	 'Shear  ','modulus,G  ',      'Membrane   ','thickness  ',
     5	 'Area of    ','section,A  ',      'Izz      ','(bending)  ',
     7	 'Ixx    ','(bending)  ',   'Polar      ','moment,J   ',
     9	 'Density    ','x-gravity  ', 'Density    ','y-gravity  ',
     1	 'Density    ','z-gravity  ',     'Lateral    ','pressure   ',
     3	 'Extra  ','stiffness  ',         'x-tension  ','(in-plane) ',
     5	 'y-tension  ','(in-plane) ',     'xy-shear   ','(in-plane) ',
     7	 'Number of  ','Gauss pts. ',     'Coef. of   ','expansion  ',
     9	 'Temperature','(uniform)  ',     'Angular    ','velocity   ',
     1	 'Temp.diff  ','top-bottom '/
      end

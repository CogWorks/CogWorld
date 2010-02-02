(clear-all)

(define-model test-temporal

(sgp :esc t :trace-detail high)

(chunk-type test slot1 slot2 slot3)

(add-dm (goal isa test))
(goal-focus goal)

(p start-timer
   =goal>
     isa test
     slot1 nil
==>
   +temporal>
     isa time
   =goal>
     slot1 running)


(p wait
   =goal>
     isa test
     slot1 running
==>
   =goal>
      slot1 done)

(spp wait :effort 8.0)


(p read
   =goal>
     isa test
     slot1 done
   =temporal>
     isa time
     ticks =ticks
==>
   !output! =ticks
   +temporal>
     isa time
   =goal>
     slot1 waiting
     slot2 =ticks
)
   

(p wait2
   =goal>
     isa test
     slot1 waiting
     slot2 =ticks
   =temporal>
     isa time
     ticks =ticks
==>
   +temporal>
     isa clear
   -goal>)

)
traffic_light ::= Green | Amber| Red | Red_amber
action ::= Stop | No_change | Start | Slow_down | Prepare_to_start

states = [Green, Amber, Red, Red_amber]

action_to_take :: traffic_light -> action
action_to_take Green = Start
action_to_take Amber = Slow_down 
action_to_take Red = Stop 
action_to_take Red_amber = Prepare_to_start 
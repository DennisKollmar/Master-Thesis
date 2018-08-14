training_root="/home/millera/masterarbeit"
if [ $# -eq 0 ]
  then
      echo
    	echo "--- No arguments supplied"
      echo "--- train.sh training-set steps training-result"
      echo
      echo "possible training-sets:"
      ls $training_root/training-sets
      echo

	exit 1;
fi

echo "-- Lernen des Set: $1 in das Result: $3"
echo 

if [ -d "$training_root/training-results/$3" ]; then
  echo "LÖSCHE altes TRAINING RESULT? $training_root/training-results/$3"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) rm -r $training_root/training-results/$3; break;;
        No ) exit;;
    esac
  done
fi
mkdir $training_root/training-results/$3

python train.py \
 --image_dir $training_root/training-sets/$1 \
 --testing_percentage 1 \
 --how_many_training_steps $2 \
 --output_labels $training_root/training-results/$3/$3.txt \
 --output_graph $training_root/training-results/$3/$3.pb \
 --summaries_dir $training_root/training-results/$3/logs \
 --bottleneck_dir $training_root/bottlenecks 2>&1 | tee -a $training_root/training-results/$3/training_log.txt

echo
echo "FERTIG"
grep 'Final test accuracy' $training_root/training-results/$3/training_log.txt
echo
echo "--------------------------------------------"
echo


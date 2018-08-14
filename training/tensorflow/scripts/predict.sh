training_root="/home/millera/masterarbeit"
TF_CPP_MIN_LOG_LEVEL=2

if [ $# -eq 0 ]
  then
      echo ""
    	echo "--- No arguments supplied"
      echo "--- predict.sh training-result prediction-set result-name"
      echo ""
      echo "possible training-results:"
      ls $training_root/training-results/
      echo ""
      echo "possible validation_sets:"
      ls $training_root/prediction-sets/
      echo ""
	exit 1;
fi

python predict.py \
--graph=$training_root/training-results/$1/$1.pb \
--labels=$training_root/training-results/$1/$1.txt \
--image_dir=$training_root/prediction-sets/$2 \
--result_file=$training_root/prediction-results/$3.csv \
--input_layer=Placeholder --output_layer=final_result

ECHO
ECHO "FERTIG"
ECHO

for i in {2001..2018}
	do
	echo "$i"
	Rscript PATH/TO/DIR/MOD13Q1/911_fortify.R	 "$i"
done

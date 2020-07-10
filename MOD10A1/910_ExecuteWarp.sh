for i in {2001..2018}
	do
	echo "$i"
	Rscript PATH/TO/MOD10A1/910_gdalWarp.R	 "$i"
done

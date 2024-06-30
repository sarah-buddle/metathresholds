results=W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods
kraken_db_name=refseq-2023-06-08-nucleotide-v2

run=illumina_250923
sample=DNA_10000_b_sub
e=pe

samples=(DNA_10000_a_sub DNA_10000_b_sub DNA_1000_a_sub DNA_1000_b_sub DNA_100_a_sub DNA_100_b_sub DNA_10_a_sub DNA_10_b_sub DNA_NC_1_sub DNA_NC_2_sub \
RNA_10000_a_sub RNA_10000_b_sub RNA_1000_a_sub RNA_1000_b_sub RNA_100_a_sub RNA_100_b_sub RNA_10_a_sub RNA_10_b_sub RNA_NC_1_sub RNA_NC_2_sub)

for sample in ${samples[@]}; do

    echo -e ${sample}_${run},,,,,${results}/kraken2/${kraken_db_name}/${sample}_${e}_${run}_${kraken_db_name}.kraken2.kraken2.report.txt \
    >> /SAN/breuerlab/BTRU-scratch/sarah/results/virus_methods/samplesheet_metathresholds.csv

done
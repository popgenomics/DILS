#!/usr/bin/bash
params_nameA=$1
input_infos=$2
input_fasta=$3
input_nLoci=$4
output_tmp=$5
output_results=$6
binpath=$7


# RNAseqFGT.sh {params.nameA} {input.infos} {input.fasta} {input.nLoci} {output.tmp} {output.results} {binpath}

x=$(cat ${input_infos}  | grep -v locusName | awk '{print $1}' | sort -n | tail -n1)
if (($x<99990)); then
	${binpath}/RNAseqFGT ${input_fasta} ${output_tmp}
	cp ${output_tmp} ${output_results}
else

	echo "${input_fasta} not in correct FASTA format (line length > 100000)" > ${output_results}
	echo "${input_fasta} not in correct FASTA format (line length > 100000)" > ${output_tmp}
fi

return 0



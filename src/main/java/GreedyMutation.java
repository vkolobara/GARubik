package main.java;

import java.util.List;

import org.jgap.Configuration;
import org.jgap.Gene;
import org.jgap.IChromosome;
import org.jgap.IGeneticOperatorConstraint;
import org.jgap.InvalidConfigurationException;
import org.jgap.Population;
import org.jgap.RandomGenerator;
import org.jgap.impl.MutationOperator;

public class GreedyMutation extends MutationOperator {

	public GreedyMutation(Configuration a_config, int a_desiredMutationRate) throws InvalidConfigurationException {
		super(a_config, a_desiredMutationRate);
	}

	@Override
	public void operate(final Population a_population, final List a_candidateChromosomes) {

		if (a_population == null || a_candidateChromosomes == null) {
			// Population or candidate chromosomes list empty:
			// nothing to do.
			// -----------------------------------------------
			return;
		}
		if (getMutationRate() == 0 && getMutationRateCalc() == null) {
			// If the mutation rate is set to zero and dynamic mutation rate is
			// disabled, then we don't perform any mutation.
			// ----------------------------------------------------------------
			return;
		}

		Configuration conf = getConfiguration();

		boolean mutate = false;
		RandomGenerator generator = conf.getRandomGenerator();
		// It would be inefficient to create copies of each Chromosome just
		// to decide whether to mutate them. Instead, we only make a copy
		// once we've positively decided to perform a mutation.
		// ----------------------------------------------------------------
		int size = Math.min(conf.getPopulationSize(), a_population.size());
		IGeneticOperatorConstraint constraint = conf.getJGAPFactory().getGeneticOperatorConstraint();
		for (int i = 0; i < size; i++) {
			IChromosome chrom = a_population.getChromosome(i);
			Gene[] genes = chrom.getGenes();
			IChromosome copyOfChromosome = null;

			mutate = (generator.nextInt(getMutationRate()) == 0);

			if (mutate) {
				if (copyOfChromosome == null) {
					// ...take a copy of it...
					// -----------------------
					copyOfChromosome = (IChromosome) chrom.clone();
					// ...add it to the candidate pool...
					// ----------------------------------
					a_candidateChromosomes.add(copyOfChromosome);
					// ...then mutate all its genes...
					// -------------------------------
					genes = copyOfChromosome.getGenes();
					// In case monitoring is active, support it.
					// -----------------------------------------
					if (m_monitorActive) {
						copyOfChromosome.setUniqueIDTemplate(chrom.getUniqueID(), 1);
					}
				}
				
				for (int j=(int) copyOfChromosome.getApplicationData() + 1; j<genes.length; j++) {
					genes[j].setToRandomValue(generator);
				}
			}

		}

	}

}

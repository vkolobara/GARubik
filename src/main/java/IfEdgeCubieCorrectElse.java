package main.java;

import org.jgap.InvalidConfigurationException;
import org.jgap.gp.CommandGene;
import org.jgap.gp.IMutateable;
import org.jgap.util.ICloneable;
import org.jgap.gp.impl.GPConfiguration;
import org.jgap.gp.impl.ProgramChromosome;

import main.scala.RubiksCube;

public class IfEdgeCubieCorrectElse extends CommandGene implements ICloneable, IMutateable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private int cubieIndex;

	public IfEdgeCubieCorrectElse(GPConfiguration a_conf, int cubieIndex) throws InvalidConfigurationException {
		super(a_conf, 2, CommandGene.IntegerClass);
		this.cubieIndex = cubieIndex;
	}

	@Override
	public CommandGene applyMutation(int index, double a_percentage) throws InvalidConfigurationException {
		IfEdgeCubieCorrectElse mutant = new IfEdgeCubieCorrectElse(getGPConfiguration(), 
				getGPConfiguration().getRandomGenerator().nextInt(12));
		return mutant;
	}
	
	@Override
	public int execute_int(ProgramChromosome c, int n, Object[] args) {
		RubiksCube cube = (RubiksCube) c.getIndividual().getApplicationData();
		
		int ret;

		if (cube.getEdgeCubie(cubieIndex).isCorrect(cube)) {
			ret = c.execute_int(n, 0, args);
		} else {
			ret = c.execute_int(n, 1, args);
		}
		
		return ret;
	}


	@Override
	public Object clone() {
		try {
			return new IfEdgeCubieCorrectElse(getGPConfiguration(), this.cubieIndex);
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public String toString() {
		return "if-edge-cubie(" + cubieIndex + ") (&1) else (&2)";
	}

}

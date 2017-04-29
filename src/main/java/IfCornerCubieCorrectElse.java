package main.java;

import org.jgap.InvalidConfigurationException;
import org.jgap.gp.CommandGene;
import org.jgap.gp.impl.GPConfiguration;
import org.jgap.gp.impl.ProgramChromosome;

import main.scala.RubiksCube;

public class IfCornerCubieCorrectElse extends CommandGene implements IMutateable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private int cubieIndex;

	public IfCornerCubieCorrectElse(GPConfiguration a_conf, int cubieIndex) throws InvalidConfigurationException {
		super(a_conf, 2, CommandGene.IntegerClass);
		this.cubieIndex = cubieIndex;
	}

	@Override
	public CommandGene applyMutation(int index, double a_percentage) throws InvalidConfigurationException {
		IfCornerCubieCorrectElse mutant = new IfCornerCubieCorrectElse(getGPConfiguration(), 
				getGPConfiguration().getRandomGenerator().nextInt(8));
		return mutant;
	}
	
	@Override
	public void execute_integer(ProgramChromosome c, int n, Object[] args) {
		RubiksCube cube = (RubiksCube) c.getApplicationData();
		
		int ret;

		if (cube.getCornerCubie(cubieIndex).isCorrect(cube)) {
			ret = c.execute_integer(n, 0, args);
		} else {
			ret = c.execute_integer(n, 1, args);
		}

		return ret;
		
	}

	@Override
	public String toString() {
		return "if-edge-cubie(" + cubieIndex + ") (&1) else (&2)";
	}

}

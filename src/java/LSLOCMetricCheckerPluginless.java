package atom_finder;

import org.eclipse.cdt.codan.core.model.IProblemWorkingCopy;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;

import ch.hsr.ifs.cdt.metriculator.model.AbstractMetricChecker;
import ch.hsr.ifs.cdt.metriculator.model.nodes.AbstractNode;
import ch.hsr.ifs.cdt.metriculator.model.nodes.FunctionNode;
import ch.hsr.ifs.cdt.metriculator.resources.MetricLabels;
import ch.hsr.ifs.cdt.metriculator.checkers.McCabeScopedASTVisitor;
import ch.hsr.ifs.cdt.metriculator.checkers.McCabeMetric;

public class LSLOCMetricCheckerPluginless extends AbstractMetricChecker{

	public static final String PREF_MCCABE_MAXIMUM_PER_FUNCTION = "max_per_function"; //$NON-NLS-1$
	public static final String MCCABE_PROBLEM_ID                = "ch.hsr.ifs.cdt.metriculator.mccabe";
	private McCabeMetric metric;

	public LSLOCMetricCheckerPluginless(){
		super(MCCABE_PROBLEM_ID);
		metric = new McCabeMetric(this, MetricLabels.McCabeMetric_name, MetricLabels.McCabeMetric_description);
	}

	@Override
	protected void processTanslationUnit(IASTTranslationUnit tu) {
		McCabeScopedASTVisitor visitor = new McCabeScopedASTVisitor(currentScopeNode, builder);
		visitor.add(this);
		tu.accept(visitor);
	}

	@Override
	public void initPreferences(IProblemWorkingCopy problem) {
		super.initPreferences(problem);

		addPreference(problem, PREF_MCCABE_MAXIMUM_PER_FUNCTION, MetricLabels.MCCABE_Maximum_Per_Function, "15"); //$NON-NLS-1$
	}

	protected void reportProblemsFor(AbstractNode node){
		if(node instanceof FunctionNode){
			Integer maxMcCabePerFunction = getPreferenceAsInteger(MCCABE_PROBLEM_ID, PREF_MCCABE_MAXIMUM_PER_FUNCTION, getFile());
			if(node.getValueOf(metric).aggregatedValue > maxMcCabePerFunction){
				reportProblem(MCCABE_PROBLEM_ID, node, maxMcCabePerFunction);
			}
		}
	}
}

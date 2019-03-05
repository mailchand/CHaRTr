Models of Decision Making
-------------------------

List of available models
------------------------

<table style="width:100%;">
<colgroup>
<col width="26%" />
<col width="22%" />
<col width="23%" />
<col width="23%" />
<col width="4%" />
</colgroup>
<thead>
<tr class="header">
<th align="right">Model</th>
<th>Description</th>
<th>Name in model list</th>
<th>Uses C Function Name</th>
<th>Involves Urgency</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">stone</td>
<td>Vanilla drift diffusion model for decision-making originally developed by stone (1960)</td>
<td>stone</td>
<td>stone</td>
<td>No</td>
</tr>
<tr class="even">
<td align="right">stoneEta</td>
<td>Drift diffusion model with variability in the drift rates. Drift rate variability is drawn from a normal distribution.</td>
<td>stoneEta</td>
<td>stoneEta</td>
<td>No</td>
</tr>
<tr class="odd">
<td align="right">stoneEtaVarTer</td>
<td>Drift Diffusion Model with variability in the drift rates and variability in the residual time that is thought to reflect sensory and motor processing delays</td>
<td>stoneEtaVarTer</td>
<td>stoneEta</td>
<td>No</td>
</tr>
<tr class="even">
<td align="right">stoneEtaVarBaseVarTer</td>
<td>Drift Diffusion Model with variability in the drift rates, variability in the baseline state before evidence comes in and variability in the residual time that is thought to reflect sensory and motor processing delays</td>
<td>stoneEtaVarBaseVarTer</td>
<td>stoneEtaVarBase</td>
<td>No</td>
</tr>
<tr class="odd">
<td align="right">ratcliff</td>
<td>Ratcliff model that involves variability in the baseline starting point and in the drift rate</td>
<td>ratcliff</td>
<td>ratcliff</td>
<td>No</td>
</tr>
<tr class="even">
<td align="right">stoneEtaDitterich</td>
<td></td>
</tr>
</tbody>
</table>

### Using this toolbox.

This toolbox is an attempt by us (Chand and Guy) to provide the legions
of researchers interested in various models of decision-making a simple
and easily used toolbox for analysis of RT and discrimination accuracy
behavior in decision-making tasks. The architecture of the toolbox is
very simple. The choosing of which model to run and the lower and upper
parameters and the

We assume that there is a reasonable working knowledge of R and C.

Stone Model
-----------

$$
x(t+1) = x(t) + v \\times dt + \\sqrt(dt) \\times s \\times N(0,1)
$$
 Where *x* is the decision variable, *v* is the drift rate, *d**t* is
the step size, *s* is the standard deviation of the noise, *N*(0, 1)
denotes the normal distribution

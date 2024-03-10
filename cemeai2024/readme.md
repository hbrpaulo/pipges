- $X$: número de peças necessárias
- $x$: número de peças disponibilizadas
- $g(X)$: perda financeira da empresa

$$
g(X)=\begin{cases}
			\text{Preço $\cdot$ (x-X)}, & X > x\\
            \text{Multa}, & c.c.
		 \end{cases}
$$

Temos que $\mathbb{E}[g(X)]$ por peça em cada VIN é

$$g(X) = g(X) \mathbb{I}(X\leq x) + g(X)\mathbb{I}(X > x)$$

$$=\text{Multa}\cdot \mathbb{I}(X\leq x) + \text{Preço $\cdot (x-X)$}\cdot\mathbb{I}(X > x)$$

$$\mathbb{E}[g(X)] = \mathbb{E}\left[\text{Multa}\cdot \mathbb{I}(X\leq x) + \text{Preço $\cdot (x-X)$}\cdot\mathbb{I}(X > x)\right]$$

$$= P(X\leq x) \cdot \text{Multa} + P(X>x)\cdot \text{Preço $\cdot (x-X)$}$$

$$= [1 - q(x)]\text{Multa} + q(x)\text{Preço}(x-\mathbb{E}[X]).$$

Segundo a administração da empresa, a multa não é escalável com o número de VINs não entregues, logo a função perda, $g(X)$, por dia é

$$\mathbb{E}[g(X)] = [1 - q(x)]\text{Multa} + nq(x)\text{Preço}(x-\mathbb{E}[X]),$$

sendo $n$ o número de VIN produzidos por dia.

Com essa expressão procuraremos por exaustão em todos os quantis observador, $\hat{q}(x)$,

$${argmin}_{\hat{q}(x)} g(X).$$

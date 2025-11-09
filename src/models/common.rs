use std::time::Instant;

#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub request_start: Instant,
    pub first_token_time: Option<Instant>,
    pub last_token_time: Option<Instant>,
    pub token_count: usize,
    pub ttft_ms: Option<u128>,
    pub total_time_ms: Option<u128>,
    pub tokens_per_second: Option<f64>,
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        Self {
            request_start: Instant::now(),
            first_token_time: None,
            last_token_time: None,
            token_count: 0,
            ttft_ms: None,
            total_time_ms: None,
            tokens_per_second: None,
        }
    }

    pub fn record_first_token(&mut self) {
        if self.first_token_time.is_none() {
            self.first_token_time = Some(Instant::now());
            self.ttft_ms = Some(self.request_start.elapsed().as_millis());
        }
    }

    pub fn record_token(&mut self) {
        self.token_count += 1;
        self.last_token_time = Some(Instant::now());
        if self.first_token_time.is_none() {
            self.record_first_token();
        }
    }

    pub fn finalize(&mut self) {
        let end_time = self.last_token_time.unwrap_or_else(Instant::now);
        self.total_time_ms = Some(self.request_start.elapsed().as_millis());
        
        if let Some(first_token_time) = self.first_token_time {
            let streaming_duration = end_time.duration_since(first_token_time);
            if !streaming_duration.is_zero() && self.token_count > 0 {
                self.tokens_per_second = Some(
                    self.token_count as f64 / streaming_duration.as_secs_f64()
                );
            }
        }
    }
}